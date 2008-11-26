{- | 
Here are the serialization and deserialization functions.

This module cooperates with the generated code to implement the Wire
instances.  The encoding is mostly documented at
<http://code.google.com/apis/protocolbuffers/docs/encoding.html>.

The user API functions are grouped into sections and documented.  The
rest are for internal use.  The main functions are 'messageGet' and
'messagePut' (and 'messageSize').  There are then several 'message*'
variants which allow for finer control and for making delimited
messages.
-}
module Text.ProtocolBuffers.WireMessage
    ( -- * User API functions
      -- ** Main encoding and decoding operations (non-delimited message encoding)
      messageSize,messagePut,messageGet,messagePutM,messageGetM
      -- **  The author's home brewed encoding (length written first to delimit message)
    , messageWithLengthSize,messageWithLengthPut,messageWithLengthGet,messageWithLengthPutM,messageWithLengthGetM
      -- ** Encoding to write or read a single message field (good for delimited messages or incremental use)
    , messageAsFieldSize,messageAsFieldPutM,messageAsFieldGetM
      -- ** The Put monad from the binary package, and a custom binary Get monad ("Text.ProtocolBuffers.Get")
    , Put,Get,runPut,runGet,runGetOnLazy,getFromBS
      -- * The Wire monad itself.  Users should beware that passing an incompatible 'FieldType' is a runtime error or fail
    , Wire(..)
      -- * The internal exports, for use by generated code and the "Text.ProtcolBuffer.Extensions" module
    , size'Varint,toWireType,toWireTag,mkWireTag
    , prependMessageSize,putSize,putVarUInt,getVarInt,putLazyByteString,splitWireTag
    , wireSizeReq,wireSizeOpt,wireSizeRep
    , wirePutReq,wirePutOpt,wirePutRep
    , wireSizeErr,wirePutErr,wireGetErr
    , getMessageWith,getBareMessageWith,wireGetEnum
    , unknownField,unknown,wireGetFromWire
    , castWord64ToDouble,castWord32ToFloat,castDoubleToWord64,castFloatToWord32
    , zzEncode64,zzEncode32,zzDecode64,zzDecode32
    ) where

import Control.Monad(when)
import Control.Monad.Error.Class(throwError,catchError)
import Control.Monad.ST
import Data.Array.ST
import Data.Bits (Bits(..))
import qualified Data.ByteString.Lazy as BS (length)
import qualified Data.Foldable as F(foldl',forM_)
import Data.List (genericLength)
import qualified Data.Set as Set(delete,null,notMember)
import Data.Typeable (Typeable(..))
-- GHC internals for getting at Double and Float representation as Word64 and Word32
-- This has been superceded by the ST array trick (ugly, but promised to work)
--import GHC.Exts (Double(D#),Float(F#),unsafeCoerce#)
--import GHC.Word (Word64(W64#)) -- ,Word32(W32#))

-- binary package
import Data.Binary.Put (Put,runPut,putWord8,putWord32le,putWord64le,putLazyByteString)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Get as Get (Result(..),Get,runGet,bytesRead,isReallyEmpty
                                       ,spanOf,skip,lookAhead
                                       ,getWord8,getWord32le,getWord64le,getLazyByteString)
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.Reflections(ReflectDescriptor(reflectDescriptorInfo,getMessageInfo)
                                       ,DescriptorInfo(..),GetMessageInfo(..))

-- External user API for writing and reading messages

-- | This computes the size of the message's fields with tags on the
-- wire with no initial tag or length (in bytes).  This is also the
-- length of the message as placed between group start and stop tags.
messageSize :: (ReflectDescriptor msg,Wire msg) => msg -> WireSize
messageSize msg = wireSize 10 msg

-- | This computes the size of the message fields as in 'messageSize'
-- and add the length of the encoded size to the total.  Thus this is
-- the the length of the message including the encoded length header,
-- but without any leading tag.
messageWithLengthSize :: (ReflectDescriptor msg,Wire msg) => msg -> WireSize
messageWithLengthSize msg = wireSize 11 msg

-- | This computes the size of the 'messageWithLengthSize' and then
-- adds the length an initial tag with the given 'FieldId'.
messageAsFieldSize :: (ReflectDescriptor msg,Wire msg) => FieldId -> msg -> WireSize
messageAsFieldSize fi msg = let headerSize = size'Varint (getWireTag (toWireTag fi 11))
                            in headerSize + messageWithLengthSize msg

-- | This is 'runPut' applied to 'messagePutM'. It result in a
-- 'ByteString' with a length of 'messageSize' bytes.
messagePut :: (ReflectDescriptor msg, Wire msg) => msg -> ByteString
messagePut msg = runPut (messagePutM msg)

-- | This is 'runPut' applied to 'messageWithLengthPutM'.  It results
-- in a 'ByteString' with a length of 'messageWithLengthSize' bytes.
messageWithLengthPut :: (ReflectDescriptor msg, Wire msg) => msg -> ByteString
messageWithLengthPut msg = runPut (messageWithLengthPutM msg)

-- | This writes just the message's fields with tags to the wire.  This
-- 'Put' monad can be composed and eventually executed with 'runPut'.
--
-- This is actually @ wirePut 10 msg @
messagePutM :: (ReflectDescriptor msg, Wire msg) => msg -> Put
messagePutM msg = wirePut 10 msg

-- | This writes the encoded length of the message's fields and then
--  the message's fields with tags to the wire.  This 'Put' monad can
--  be composed and eventually executed with 'runPut'.
--
-- This is actually @ wirePut 11 msg @
messageWithLengthPutM :: (ReflectDescriptor msg, Wire msg) => msg -> Put
messageWithLengthPutM msg = wirePut 11 msg

-- | This writes an encoded wire tag with the given 'FieldId' and then
--  the encoded length of the message's fields and then the message's
--  fields with tags to the wire.  This 'Put' monad can be composed
--  and eventually executed with 'runPut'.
messageAsFieldPutM :: (ReflectDescriptor msg, Wire msg) => FieldId -> msg -> Put
messageAsFieldPutM fi msg = let wireTag = toWireTag fi 11
                            in wirePutReq wireTag 11 msg

-- | This consumes the 'ByteString' to decode a message.  It assumes
-- the 'ByteString' is merely a sequence of the tagged fields of the
-- message, and consumes until a group stop tag is detected or the
-- entire input is consumed.  Any 'ByteString' past the end of the
-- stop tag is returned as well.
--
-- This is 'runGetOnLazy' applied to 'messageGetM'.
messageGet :: (ReflectDescriptor msg, Wire msg) => ByteString -> Either String (msg,ByteString)
messageGet bs = runGetOnLazy (messageGetM) bs

-- | This 'runGetOnLazy' applied to 'messageWithLengthGetM'.
--
-- This first reads the encoded length of the message and will then
-- succeed when it has consumed precisely this many additional bytes.
-- The 'ByteString' after this point will be returned.
messageWithLengthGet :: (ReflectDescriptor msg, Wire msg) => ByteString -> Either String (msg,ByteString)
messageWithLengthGet bs = runGetOnLazy (messageWithLengthGetM) bs

-- | This reads the tagged message fields until the stop tag or the
-- end of input is reached.
--
-- This is actually @ wireGet 10 msg @
messageGetM :: (ReflectDescriptor msg, Wire msg) => Get msg
messageGetM = wireGet 10

-- | This reads the encoded message length and then the message.
--
-- This is actually @ wireGet 11 msg @
messageWithLengthGetM :: (ReflectDescriptor msg, Wire msg) => Get msg
messageWithLengthGetM = wireGet 11

-- | This reads a wire tag (must be of type '2') to get the 'FieldId'.
-- Then the encoded message length is read, followed by the message
-- itself.  Both the 'FieldId' and the message are returned.
--
-- This allows for incremental reading and processing.
messageAsFieldGetM :: (ReflectDescriptor msg, Wire msg) => Get (FieldId,msg)
messageAsFieldGetM = do
  wireTag <- fmap WireTag getVarInt
  let (fieldId,wireType) = splitWireTag wireTag
  when (wireType /= 2) (throwError $ "messageAsFieldGetM: wireType was not 2 "++show (fieldId,wireType))
  msg <- wireGet 11
  return (fieldId,msg)

-- more functions

-- | This is 'runGetOnLazy' with the 'Left' results converted to
-- 'error' calls and the trailing 'ByteString' discarded.  This use of
-- runtime errors is discouraged, but may be convenient.
getFromBS :: Get r -> ByteString -> r
getFromBS parser bs = case runGetOnLazy parser bs of
                        Left msg -> error msg
                        Right (r,_) -> r

-- This is like 'runGet', without the ability to pass in more input
-- beyond the initial ByteString.  Thus the 'ByteString' argument is
-- taken to be the entire input.  To be able to incrementally feed in
-- more input you should use 'runGet' and respond to 'Partial'
-- differently.
runGetOnLazy :: Get r -> ByteString -> Either String (r,ByteString)
runGetOnLazy parser bs = resolve (runGet parser bs)
  where resolve :: Result r -> Either String (r,ByteString)
        resolve (Failed i s) = Left ("Failed at "++show i++" : "++s)
        resolve (Finished bsOut _i r) = Right (r,bsOut)
        resolve (Partial op) = resolve (op Nothing)

-- | Used in generated code.
prependMessageSize :: WireSize -> WireSize
prependMessageSize n = n + size'Varint n

{-# INLINE wirePutReq #-}
-- | Used in generated code.
wirePutReq :: Wire b => WireTag -> FieldType -> b -> Put
wirePutReq wireTag 10 b = let startTag = getWireTag wireTag
                              endTag = succ startTag
                          in putVarUInt startTag >> wirePut 10 b >> putVarUInt endTag
wirePutReq wireTag fieldType b = putVarUInt (getWireTag wireTag) >> wirePut fieldType b

{-# INLINE wirePutOpt #-}
-- | Used in generated code.
wirePutOpt :: Wire b => WireTag -> FieldType -> Maybe b -> Put
wirePutOpt _wireTag _fieldType Nothing = return ()
wirePutOpt wireTag fieldType (Just b) = wirePutReq wireTag fieldType b 

{-# INLINE wirePutRep #-}
-- | Used in generated code.
wirePutRep :: Wire b => WireTag -> FieldType -> Seq b -> Put
wirePutRep wireTag fieldType bs = F.forM_ bs (\b -> wirePutReq wireTag fieldType b)

{-# INLINE wireSizeReq #-}
-- | Used in generated code.
wireSizeReq :: Wire b => Int64 -> FieldType -> b -> Int64
wireSizeReq tagSize 10 v = tagSize + wireSize 10 v + tagSize
wireSizeReq tagSize  i v = tagSize + wireSize i v

{-# INLINE wireSizeOpt #-}
-- | Used in generated code.
wireSizeOpt :: Wire b => Int64 -> FieldType -> Maybe b -> Int64
wireSizeOpt _tagSize _i Nothing = 0
wireSizeOpt tagSize i (Just v) = wireSizeReq tagSize i v

{-# INLINE wireSizeRep #-}
-- | Used in generated code.
wireSizeRep :: Wire b => Int64 -> FieldType -> Seq b -> Int64
wireSizeRep tagSize i s = F.foldl' (\n v -> n + wireSizeReq tagSize i v) 0 s

-- | Used in generated code.
putSize :: WireSize -> Put
putSize = putVarUInt

toWireTag :: FieldId -> FieldType -> WireTag
toWireTag fieldId fieldType
    = ((fromIntegral . getFieldId $ fieldId) `shiftL` 3) .|. (fromIntegral . getWireType . toWireType $ fieldType)

mkWireTag :: FieldId -> WireType -> WireTag
mkWireTag fieldId fieldType
    = ((fromIntegral . getFieldId $ fieldId) `shiftL` 3) .|. (fromIntegral . getWireType $ fieldType)

splitWireTag :: WireTag -> (FieldId,WireType)
splitWireTag (WireTag wireTag) = ( FieldId . fromIntegral $ wireTag `shiftR` 3
                                 , WireType . fromIntegral $ wireTag .&. 7 )



-- getMessageWith assumes the wireTag for the message, if it existed, has already been read.
-- getMessageWith assumes that it still needs to read the Varint encoded length of the message.
getMessageWith :: (Mergeable message, ReflectDescriptor message)
               => (WireTag -> FieldId -> WireType -> message -> Get message)
               -> Get message
getMessageWith updater = do
  messageLength <- getVarInt
  start <- bytesRead
  let stop = messageLength+start
      -- switch from go to go' once all the required fields have been found
      go reqs message | Set.null reqs = go' message
                      | otherwise = do
        here <- bytesRead
        case compare stop here of
          EQ -> notEnoughData messageLength start
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
                reqs' = Set.delete wireTag reqs
            updater wireTag fieldId wireType message >>= go reqs'
      go' message = do
        here <- bytesRead
        case compare stop here of
          EQ -> return message
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            updater wireTag fieldId wireType message >>= go'
  go required initialMessage
 where
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required}) = getMessageInfo initialMessage
  notEnoughData messageLength start =
      throwError ("Text.ProtocolBuffers.WireMessage.getMessageWith: Required fields missing when processing "
                  ++ (show . descName . reflectDescriptorInfo $ initialMessage)
                  ++ " at (messageLength,start) == " ++ show (messageLength,start))
  tooMuchData messageLength start here =
      throwError ("Text.ProtocolBuffers.WireMessage.getMessageWith: overran expected length when processing"
                  ++ (show . descName . reflectDescriptorInfo $ initialMessage)
                  ++ " at  (messageLength,start,here) == " ++ show (messageLength,start,here))

-- | Used by generated code
-- getBareMessageWith assumes the wireTag for the message, if it existed, has already been read.
-- getBareMessageWith assumes that it does needs to read the Varint encoded length of the message.
-- getBareMessageWith will consume the entire ByteString it is operating on, or until it
-- finds any STOP_GROUP tag (wireType == 4)
getBareMessageWith :: (Mergeable message, ReflectDescriptor message)
                   => (WireTag -> FieldId -> WireType -> message -> Get message) -- handle wireTags that are unknown or produce errors
                   -> Get message
getBareMessageWith updater = go required initialMessage
 where
  go reqs message | Set.null reqs = go' message
                  | otherwise = do
    done <- isReallyEmpty
    if done then notEnoughData
      else do
        wireTag <- fmap WireTag getVarInt -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if wireType == 4 then notEnoughData -- END_GROUP too soon
          else let reqs' = Set.delete wireTag reqs
               in updater wireTag fieldId wireType message >>= go reqs'
  go' message = do
    done <- isReallyEmpty
    if done then return message
      else do
        wireTag <- fmap WireTag getVarInt -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if wireType == 4 then return message
          else updater wireTag fieldId wireType message >>= go'
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required}) = getMessageInfo initialMessage
  notEnoughData = throwError ("Text.ProtocolBuffers.WireMessage.getBareMessageWith: Required fields missing when processing "
                              ++ (show . descName . reflectDescriptorInfo $ initialMessage))

unknownField :: Typeable a => a -> FieldId -> Get a
unknownField msg fieldId = do 
  here <- bytesRead
  throwError ("Impossible? Text.ProtocolBuffers.WireMessage.unknownField"
              ++"\n  Updater for "++show (typeOf msg)++" claims there is an unknown field id on wire: "++show fieldId
              ++"\n  at a position just before byte location "++show here)


unknown :: (Typeable a,ReflectDescriptor a) => FieldId -> WireType -> a -> Get a
unknown fieldId wireType initialMessage = do
  here <- bytesRead
  throwError ("Text.ProtocolBuffers.WireMessage.unknown: Unknown field found or failure parsing field (e.g. unexpected Enum value):"
              ++ "(message type name,field id number,wire type code,bytes read) == "
              ++ show (typeOf initialMessage,fieldId,wireType,here) ++ " when processing "
              ++ (show . descName . reflectDescriptorInfo $ initialMessage))

{-# INLINE castWord32ToFloat #-}
castWord32ToFloat :: Word32 -> Float
--castWord32ToFloat (W32# w) = F# (unsafeCoerce# w)
--castWord32ToFloat x = unsafePerformIO $ alloca $ \p -> poke p x >> peek (castPtr p)
castWord32ToFloat x = runST (newArray (0::Int,0) x >>= castSTUArray >>= flip readArray 0)
{-# INLINE castFloatToWord32 #-}
castFloatToWord32 :: Float -> Word32
--castFloatToWord32 (F# f) = W32# (unsafeCoerce# f)
castFloatToWord32 x = runST (newArray (0::Int,0) x >>= castSTUArray >>= flip readArray 0)

{-# INLINE castWord64ToDouble #-}
castWord64ToDouble :: Word64 -> Double
-- castWord64ToDouble (W64# w) = D# (unsafeCoerce# w)
castWord64ToDouble x = runST (newArray (0::Int,0) x >>= castSTUArray >>= flip readArray 0)
{-# INLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
-- castDoubleToWord64 (D# d) = W64# (unsafeCoerce# d)
castDoubleToWord64 x = runST (newArray (0::Int,0) x >>= castSTUArray >>= flip readArray 0)

-- These error handlers are exported to the generated code
wireSizeErr :: Typeable a => FieldType -> a -> WireSize
wireSizeErr ft x = error $ concat [ "Impossible? wireSize field type mismatch error: Field type number ", show ft
                                  , " does not match internal type ", show (typeOf x) ]
wirePutErr :: Typeable a => FieldType -> a -> Put
wirePutErr ft x = fail $ concat [ "Impossible? wirePut field type mismatch error: Field type number ", show ft
                                , " does not match internal type ", show (typeOf x) ]
wireGetErr :: Typeable a => FieldType -> Get a
wireGetErr ft = answer where
  answer = throwError $ concat [ "Impossible? wireGet field type mismatch error: Field type number ", show ft
                               , " does not match internal type ", show (typeOf (undefined `asTypeOf` typeHack answer)) ]
  typeHack :: Get a -> a
  typeHack = undefined

instance Wire Double where
  wireSize {- TYPE_DOUBLE   -} 1      _ = 8
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_DOUBLE   -} 1      x = putWord64le (castDoubleToWord64 x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_DOUBLE   -} 1        = fmap castWord64ToDouble getWord64le
  wireGet ft = wireGetErr ft

instance Wire Float where
  wireSize {- TYPE_FLOAT    -} 2      _ = 4
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_FLOAT    -} 2      x = putWord32le (castFloatToWord32 x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_FLOAT    -} 2        = fmap castWord32ToFloat getWord32le
  wireGet ft = wireGetErr ft

instance Wire Int64 where
  wireSize {- TYPE_INT64    -} 3      x = size'Varint x
  wireSize {- TYPE_SINT64   -} 18     x = size'Varint (zzEncode64 x)
  wireSize {- TYPE_SFIXED64 -} 16     _ = 8
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_INT64    -} 3      x = putVarSInt x
  wirePut  {- TYPE_SINT64   -} 18     x = putVarUInt (zzEncode64 x)
  wirePut  {- TYPE_SFIXED64 -} 16     x = putWord64le (fromIntegral x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_INT64    -} 3        = getVarInt
  wireGet  {- TYPE_SINT64   -} 18       = fmap zzDecode64 getVarInt
  wireGet  {- TYPE_SFIXED64 -} 16       = fmap fromIntegral getWord64le
  wireGet ft = wireGetErr ft

instance Wire Int32 where
  wireSize {- TYPE_INT32    -} 5      x = size'Varint x
  wireSize {- TYPE_SINT32   -} 17     x = size'Varint (zzEncode32 x)
  wireSize {- TYPE_SFIXED32 -} 15     _ = 4
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_INT32    -} 5      x = putVarSInt x
  wirePut  {- TYPE_SINT32   -} 17     x = putVarUInt (zzEncode32 x)
  wirePut  {- TYPE_SFIXED32 -} 15     x = putWord32le (fromIntegral x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_INT32    -} 5        = getVarInt
  wireGet  {- TYPE_SINT32   -} 17       = fmap zzDecode32 getVarInt
  wireGet  {- TYPE_SFIXED32 -} 15       = fmap fromIntegral getWord32le
  wireGet ft = wireGetErr ft

instance Wire Word64 where
  wireSize {- TYPE_UINT64   -} 4      x = size'Varint x
  wireSize {- TYPE_FIXED64  -} 6      _ = 8
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_UINT64   -} 4      x = putVarUInt x
  wirePut  {- TYPE_FIXED64  -} 6      x = putWord64le x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_FIXED64  -} 6        = getWord64le
  wireGet  {- TYPE_UINT64   -} 4        = getVarInt
  wireGet ft = wireGetErr ft

instance Wire Word32 where
  wireSize {- TYPE_UINT32   -} 13     x = size'Varint x
  wireSize {- TYPE_FIXED32  -} 7      _ = 4
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_UINT32   -} 13     x = putVarUInt x
  wirePut  {- TYPE_FIXED32  -} 7      x = putWord32le x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_UINT32   -} 13       = getVarInt
  wireGet  {- TYPE_FIXED32  -} 7        = getWord32le
  wireGet ft = wireGetErr ft

instance Wire Bool where
  wireSize {- TYPE_BOOL     -} 8      _ = 1
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_BOOL     -} 8  False = putWord8 0
  wirePut  {- TYPE_BOOL     -} 8  True  = putWord8 1 -- google's wire_format_inl.h
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_BOOL     -} 8        = do
    x <- getVarInt :: Get Int32 -- google's wire_format_inl.h line 97
    case x of
      0 -> return False
      x' | x' < 128 -> return True
      _ -> throwError ("TYPE_BOOL read failure : " ++ show x)
  wireGet ft = wireGetErr ft

instance Wire Utf8 where
-- items of TYPE_STRING is already in a UTF8 encoded Data.ByteString.Lazy
  wireSize {- TYPE_STRING   -} 9      x = prependMessageSize $ BS.length (utf8 x)
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_STRING   -} 9      x = putVarUInt (BS.length (utf8 x)) >> putLazyByteString (utf8 x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_STRING   -} 9        = getVarInt >>= getLazyByteString >>= verifyUtf8
  wireGet ft = wireGetErr ft

instance Wire ByteString where
-- items of TYPE_BYTES is an untyped binary Data.ByteString.Lazy
  wireSize {- TYPE_BYTES    -} 12     x = prependMessageSize $ BS.length x
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_BYTES    -} 12     x = putVarUInt (BS.length x) >> putLazyByteString x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_BYTES    -} 12       = getVarInt >>= getLazyByteString
  wireGet ft = wireGetErr ft

-- Wrap a protocol-buffer Enum in fromEnum or toEnum and serialize the Int:
instance Wire Int where
  wireSize {- TYPE_ENUM    -} 14      x = size'Varint x
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_ENUM    -} 14      x = putVarSInt x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_ENUM    -} 14        = getVarInt
  wireGet ft = wireGetErr ft

{-# INLINE verifyUtf8 #-}
verifyUtf8 :: ByteString -> Get Utf8
verifyUtf8 bs = case isValidUTF8 bs of
                  Nothing -> return (Utf8 bs)
                  Just i -> throwError $ "Text.ProtocolBuffers.WireMessage.verifyUtf8: ByteString is not valid utf8 at position "++show i

{-# INLINE wireGetEnum #-}
wireGetEnum :: (Typeable e, Enum e) => (Int -> Maybe e) -> Get e
wireGetEnum toMaybe'Enum = do
  int <- wireGet 14
  case toMaybe'Enum int of
    Just v -> return v
    Nothing -> throwError (msg ++ show int)
 where msg = "Bad wireGet of Enum "++show (typeOf (undefined `asTypeOf` typeHack toMaybe'Enum))++", unrecognized Int value is "
       typeHack :: (Int -> Maybe e) -> e
       typeHack f = maybe undefined id (f undefined)

-- This will have to examine the value of positive numbers to get the size
{-# INLINE size'Varint #-}
size'Varint :: (Bits a,Integral a) => a -> Int64
size'Varint b = case compare b 0 of
                  LT -> 10 -- fromIntegral (divBy (bitSize b) 7)
                  EQ -> 1
                  GT -> genericLength . takeWhile (0<) . iterate (`shiftR` 7) $ b

{- unused since I started casting all negative values to Int64
{-# INLINE divBy #-}
divBy :: (Ord a, Integral a) => a -> a -> a
divBy a b = let (q,r) = quotRem (abs a) b
            in if r==0 then q else succ q
-}

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-
-- The above is tricky, so the testing roundtrips and versus examples is needed:
testZZ :: Bool
testZZ = and (concat testsZZ)
  where testsZZ = [ map (\v -> v ==zzEncode64 (zzDecode64 v)) values
                  , map (\v -> v ==zzEncode32 (zzDecode32 v)) values
                  , map (\v -> v ==zzDecode64 (zzEncode64 v)) values
                  , map (\v -> v ==zzDecode32 (zzEncode32 v)) values
                  , [ zzEncode32 minBound == maxBound
                    , zzEncode32 maxBound == pred maxBound
                    , zzEncode64 minBound == maxBound
                    , zzEncode64 maxBound == pred maxBound
                    , zzEncode64 0 == 0,    zzEncode32 0 == 0
                    , zzEncode64 (-1) == 1, zzEncode32 (-1) == 1
                    , zzEncode64 1 == 2,    zzEncode32 1 == 2
                    ] ]
        values :: (Bounded a,Integral a) => [a]
        values = [minBound,div minBound 2,-3,-2,-1,0,1,2,3,div maxBound 2, maxBound]
-}

{-# INLINE getVarInt #-}
getVarInt :: (Integral a, Bits a) => Get a
getVarInt = do -- optimize first read instead of calling (go 0 0)
  b <- getWord8
  if testBit b 7 then go 7 (fromIntegral (b .&. 0x7F))
    else return (fromIntegral b)
 where
  go n val = do
    b <- getWord8
    if testBit b 7 then go (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else return (val .|. ((fromIntegral b) `shiftL` n))

-- This can be used on any Integral type and is needed for signed types; unsigned can use putVarUInt below.
-- This has been changed to handle only up to 64 bit integral values (to match documentation).
{-# INLINE putVarSInt #-}
putVarSInt :: (Typeable a, Integral a, Bits a) => a -> Put
putVarSInt bIn =
  case compare bIn 0 of
    LT -> let b :: Int64 -- upcast to 64 bit to match documentation of 10 bytes for all negative values
              b = fromIntegral bIn
--               len = divBy (bitSize b) 7               -- (pred len)*7 < bitSize b <= len*7
--               last'Size = (bitSize b)-((pred len)*7)  -- at least 1 and at most 7
--               last'Mask = pred (1 `shiftL` last'Size) -- at least 1 and at most 255
              len :: Int
              len = 10                                -- (pred 10)*7 < 64 <= 10*7
--            last'Size = 1                           -- 64 - (pred 10)*7
              last'Mask = 1                           -- pred (1 `shiftL` 1)
              go i 1 = putWord8 (fromIntegral (i .&. last'Mask))
              go i n = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7) (pred n)
          in go b len
    EQ -> putWord8 0
    GT -> putVarUInt bIn

-- This should be used on unsigned Integral types only (not checked)
{-# INLINE putVarUInt #-}
putVarUInt :: (Integral a, Bits a) => a -> Put
putVarUInt b = let go i | i < 0x80 = putWord8 (fromIntegral i)
                        | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7)
               in go b

-- | This reads in the raw bytestring corresponding to an field known
-- only through the wiretag's 'FieldId' and 'WireType'.
wireGetFromWire :: FieldId -> WireType -> Get ByteString
wireGetFromWire fi wt = getLazyByteString =<< calcLen where
  calcLen = case wt of
              0 -> lenOf (spanOf (>=128) >> skip 1)
              1 -> return 8
              2 -> lookAhead $ do
                     here <- bytesRead
                     len <- getVarInt
                     there <- bytesRead
                     return ((there-here)+len)
              3 -> lenOf (skipGroup fi)
              4 -> throwError $ "Cannot wireGetFromWire with wireType of STOP_GROUP: "++show (fi,wt)
              5 -> return 4
              wtf -> throwError $ "Invalid wire type (expected 0,1,2,3,or 5) found: "++show (fi,wtf)
  lenOf g = do here <- bytesRead
               there <- lookAhead (g >> bytesRead)
               return (there-here)
          
-- | After a group start tag with the given 'FieldId' this will skip
-- ahead in the stream past the end tag of that group.  Used by
-- 'wireGetFromWire' to help compule the length of an unknown field
-- when loading an extension.
skipGroup :: FieldId -> Get ()
skipGroup start_fi = go where
  go = do
    (fieldId,wireType) <- fmap (splitWireTag . WireTag) getVarInt
    case wireType of
      0 -> spanOf (>=128) >> skip 1 >> go
      1 -> skip 8 >> go
      2 -> getVarInt >>= skip >> go
      3 -> skipGroup fieldId >> go
      4 | start_fi /= fieldId -> throwError $ "skipGroup failed, fieldId mismatch bewteen START_GROUP and STOP_GROUP: "++show (start_fi,(fieldId,wireType))
        | otherwise -> return ()
      5 -> skip 4 >> go
      wtf -> throwError $ "Invalid wire type (expected 0,1,2,3,4,or 5) found: "++show (fieldId,wtf)

{-
  enum WireType {
    WIRETYPE_VARINT           = 0,
    WIRETYPE_FIXED64          = 1,
    WIRETYPE_LENGTH_DELIMITED = 2,
    WIRETYPE_START_GROUP      = 3,
    WIRETYPE_END_GROUP        = 4,
    WIRETYPE_FIXED32          = 5, };

    TYPE_DOUBLE         = 1;
    TYPE_FLOAT          = 2;
    TYPE_INT64          = 3;
    TYPE_UINT64         = 4;
    TYPE_INT32          = 5;
    TYPE_FIXED64        = 6;
    TYPE_FIXED32        = 7;
    TYPE_BOOL           = 8;
    TYPE_STRING         = 9;
    TYPE_GROUP          = 10;  // Tag-delimited aggregate.
    TYPE_MESSAGE        = 11;
    TYPE_BYTES          = 12;
    TYPE_UINT32         = 13;
    TYPE_ENUM           = 14;
    TYPE_SFIXED32       = 15;
    TYPE_SFIXED64       = 16;
    TYPE_SINT32         = 17;
    TYPE_SINT64         = 18; -}
-- http://code.google.com/apis/protocolbuffers/docs/encoding.html
toWireType :: FieldType -> WireType
toWireType  1 =  1
toWireType  2 =  5
toWireType  3 =  0
toWireType  4 =  0
toWireType  5 =  0
toWireType  6 =  1
toWireType  7 =  5
toWireType  8 =  0
toWireType  9 =  2
toWireType 10 =  3 -- START_GROUP
toWireType 11 =  2
toWireType 12 =  2
toWireType 13 =  0
toWireType 14 =  0
toWireType 15 =  5
toWireType 16 =  1
toWireType 17 =  0
toWireType 18 =  0
toWireType  x = error $ "Text.ProcolBuffers.Basic.toWireType: Bad FieldType: "++show x

{-

-- getMessageWith assumes the wireTag for the message, if it existed, has already been read.
-- getMessageWith assumes that it still needs to read the Varint encoded length of the message.
getMessageWith :: (Mergeable message, ReflectDescriptor message)
               => (FieldId -> WireType -> message -> Get message) -- handle wireTags that updater cannot
               -> (FieldId -> message -> Get message)             -- handles "allowed" wireTags
               -> Get message
getMessageWith punt updater = do
  messageLength <- getVarInt
  start <- bytesRead
  let stop = messageLength+start
      -- switch from go to go' once all the required fields have been found
      go reqs message | Set.null reqs = go' message
                      | otherwise = do
        here <- bytesRead
        case compare stop here of
          EQ -> notEnoughData messageLength start
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed
              then punt fieldId wireType message >>= go reqs
              else let reqs' = Set.delete wireTag reqs
                   in catchError (updater fieldId message) 
                                 (\_ -> punt fieldId wireType message)
                        >>= go reqs'
      go' message = do
        here <- bytesRead
        case compare stop here of
          EQ -> return message
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed
              then punt fieldId wireType message >>= go'
              else catchError (updater fieldId message)
                              (\_ -> punt fieldId wireType message)
                     >>= go'
  go required initialMessage
 where
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  notEnoughData messageLength start =
      fail ("Text.ProtocolBuffers.WireMessage.getMessageWith: Required fields missing when processing "
            ++ (show . descName . reflectDescriptorInfo $ initialMessage)
            ++ " at (messageLength,start) == " ++ show (messageLength,start))
  tooMuchData messageLength start here =
      fail ("Text.ProtocolBuffers.WireMessage.getMessageWith: overran expected length when processing"
            ++ (show . descName . reflectDescriptorInfo $ initialMessage)
            ++ " at  (messageLength,start,here) == " ++ show (messageLength,start,here))

-- | Used by generated code
-- getBareMessageWith assumes the wireTag for the message, if it existed, has already been read.
-- getBareMessageWith assumes that it does needs to read the Varint encoded length of the message.
-- getBareMessageWith will consume the entire ByteString it is operating on, or until it
-- finds any STOP_GROUP tag
getBareMessageWith :: (Mergeable message, ReflectDescriptor message)
                   => (FieldId -> WireType -> message -> Get message) -- handle wireTags that updater cannot
                   -> (FieldId -> message -> Get message)             -- handles "allowed" wireTags
                   -> Get message
getBareMessageWith punt updater = go required initialMessage
 where
  go reqs message | Set.null reqs = go' message
                  | otherwise = do
    done <- isReallyEmpty
    if done then notEnoughData
      else do
        wireTag <- fmap WireTag getVarInt -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if wireType == 4 then notEnoughData -- END_GROUP too soon
          else if Set.notMember wireTag allowed
                 then punt fieldId wireType message >>= go reqs
                 else let reqs' = Set.delete wireTag reqs
                      in catchError (updater fieldId message)
                                    (\_ -> punt fieldId wireType message)
                           >>= go reqs'
  go' message = do
    done <- isReallyEmpty
    if done then return message
      else do
        wireTag <- fmap WireTag getVarInt -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag -- WIRETYPE_END_GROUP
        if wireType == 4 then return message
          else if Set.notMember wireTag allowed
                 then punt fieldId wireType message >>= go'
                 else catchError (updater fieldId message)
                                 (\_ -> punt fieldId wireType message)
                        >>= go'
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  notEnoughData = fail ("Text.ProtocolBuffers.WireMessage.getBareMessageWith: Required fields missing when processing "
                        ++ (show . descName . reflectDescriptorInfo $ initialMessage))


-}
{-
-- getMessageWith assumes the wireTag for the message, if it existed, has already been read.
-- getMessageWith assumes that it still needs to read the Varint encoded length of the message.
getMessageWith :: (Mergeable message, ReflectDescriptor message)
               => (FieldId -> message -> Get message)             -- handles "allowed" wireTags including known extensions
               -> Maybe (FieldId -> WireType -> message -> Get message) -- handle extension wireTags that updater does not
               -> (FieldId -> WireType -> message -> Get message) -- handle wireTags that are unknown or produce errors
               -> Get message
getMessageWith updater mayExt punt = do
  messageLength <- getVarInt
  start <- bytesRead
  let stop = messageLength+start
      getExt = case mayExt of
                 Nothing -> punt
                 Just loadExt -> 
                   (\fieldId wireType msg -> catchError (loadExt fieldId wireType msg)
                                                        (\_ -> punt fieldId wireType msg))
      -- switch from go to go' once all the required fields have been found
      go reqs message | Set.null reqs = go' message
                      | otherwise = do
        here <- bytesRead
        case compare stop here of
          EQ -> notEnoughData messageLength start
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed
              then getExt fieldId wireType message >>= go reqs
              else let reqs' = Set.delete wireTag reqs
                   in catchError (updater fieldId message)
                                 (\_ -> punt fieldId wireType message)
                        >>= go reqs'
      go' message = do
        here <- bytesRead
        case compare stop here of
          EQ -> return message
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed
              then getExt fieldId wireType message >>= go'
              else catchError (updater fieldId message)
                              (\_ -> punt fieldId wireType message)
                     >>= go'
  go required initialMessage
 where
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  notEnoughData messageLength start =
      throwError ("Text.ProtocolBuffers.WireMessage.getMessageWith: Required fields missing when processing "
                  ++ (show . descName . reflectDescriptorInfo $ initialMessage)
                  ++ " at (messageLength,start) == " ++ show (messageLength,start))
  tooMuchData messageLength start here =
      throwError ("Text.ProtocolBuffers.WireMessage.getMessageWith: overran expected length when processing"
                  ++ (show . descName . reflectDescriptorInfo $ initialMessage)
                  ++ " at  (messageLength,start,here) == " ++ show (messageLength,start,here))

-- | Used by generated code
-- getBareMessageWith assumes the wireTag for the message, if it existed, has already been read.
-- getBareMessageWith assumes that it does needs to read the Varint encoded length of the message.
-- getBareMessageWith will consume the entire ByteString it is operating on, or until it
-- finds any STOP_GROUP tag
getBareMessageWith :: (Mergeable message, ReflectDescriptor message)
                   => (FieldId -> message -> Get message)             -- handles "allowed" wireTags including known extensions
                   -> Maybe (FieldId -> WireType -> message -> Get message) -- handle extension wireTags that updater does not
                   -> (FieldId -> WireType -> message -> Get message) -- handle wireTags that are unknown or produce errors
                   -> Get message
getBareMessageWith updater mayExt punt = go required initialMessage
 where
  getExt = case mayExt of
             Nothing -> punt
             Just loadExt -> 
               (\fieldId wireType msg -> catchError (loadExt fieldId wireType msg)
                                                    (\_ -> punt fieldId wireType msg))
  go reqs message | Set.null reqs = go' message
                  | otherwise = do
    done <- isReallyEmpty
    if done then notEnoughData
      else do
        wireTag <- fmap WireTag getVarInt -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if wireType == 4 then notEnoughData -- END_GROUP too soon
          else if Set.notMember wireTag allowed
                 then getExt fieldId wireType message >>= go reqs
                 else let reqs' = Set.delete wireTag reqs
                      in catchError (updater fieldId message)
                                    (\_ -> punt fieldId wireType message)
                           >>= go reqs'
  go' message = do
    done <- isReallyEmpty
    if done then return message
      else do
        wireTag <- fmap WireTag getVarInt -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag -- WIRETYPE_END_GROUP
        if wireType == 4 then return message
          else if Set.notMember wireTag allowed
                 then getExt fieldId wireType message >>= go'
                 else catchError (updater fieldId message)
                                 (\_ -> punt fieldId wireType message)
                        >>= go'
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  notEnoughData = throwError ("Text.ProtocolBuffers.WireMessage.getBareMessageWith: Required fields missing when processing "
                              ++ (show . descName . reflectDescriptorInfo $ initialMessage))

unknownField :: FieldId -> Get a
unknownField fieldId = do 
  here <- bytesRead
  throwError ("Impossible? Text.ProtocolBuffers.WireMessage.unknownField"
              ++"\n  Updater claims there is an unknown field id on wire: "++show fieldId
              ++"\n  at a position just before byte location "++show here)
-}