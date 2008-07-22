-- http://code.google.com/apis/protocolbuffers/docs/encoding.html
module Text.ProtocolBuffers.WireMessage(Wire(..)) where

{- The point of this module:

There needs to be a way to represent the bytes in a form that is
in-between the type specific messages and the single string of bytes.

Encoding and Deconding WireMessage does not respect ordering between
different FieldId but must respect ordering of repeated FieldId
values.

Converting WireData to and from the actual types (e.g. sint64) will be
done elsewhere.

Converting a WireData to a message type will require code generation
of a class instance.

-}
import Text.ProtocolBuffers.Basic

import Data.Bits(Bits(..))
import qualified Data.ByteString.Lazy as BS (length,pack,unpack)
import Data.Generics(Data(..),Typeable(..))
import Data.List(unfoldr,genericLength)
import Data.Map(Map,unionWith)
import Data.Monoid(Monoid(..))
import Data.Word(Word8)

import Data.Binary.Put as Put
import Data.Binary.Get as Get
import qualified Data.Binary.Builder as Build

import GHC.Exts
import GHC.Word
import Numeric


-- --
tagSize :: Int64
tagSize = 4

-- The first Int argument is fromEnum on
-- Text.DescriptorProtos.FieldDescriptorProto.Type.  The values of the
-- Int parameters cannot change without breaking all serialized
-- protocol buffers.
class Wire b where
  {-# INLINE wireSize #-}
  wireSize :: Int -> b -> Int64
  {-# INLINE wirePut #-}
  wirePut :: Int -> b -> Put
  {-# INLINE wireGet #-}
  wireGet :: Int -> Get b

instance Wire Double where
  wireSize {- TYPE_DOUBLE -} 1     _ = 8
  wirePut {- TYPE_DOUBLE -} 1 (D# d) = putWord64be (W64# (unsafeCoerce# d))
  wireGet {- TYPE_DOUBLE -} 1        = fmap (\(W64# w) -> D# (unsafeCoerce# w)) getWord64be

instance Wire Float where
  wireSize {- TYPE_FLOAT -} 2      _ = 4
  wirePut {- TYPE_FLOAT -} 2  (F# f) = putWord32be (W32# (unsafeCoerce# f))
  wireGet {- TYPE_FLOAT -} 2         = fmap (\(W32# w) -> F# (unsafeCoerce# w)) getWord32be

instance Wire Int64 where
  wireSize {- TYPE_INT64 -} 3      x = size'Varint x
  wireSize {- TYPE_SINT64 -} 18    x = size'Varint (zzEncode64 x)
  wireSize {- TYPE_SFIXED64 -} 16  _ = 8
  wirePut {- TYPE_INT64 -} 3       x = putVarSInt x
  wirePut {- TYPE_SINT64 -} 18     x = putVarUInt (zzEncode64 x)
  wirePut {- TYPE_SFIXED64 -} 16   x = putWord64be (fromIntegral x)
  wireGet {- TYPE_INT64 -} 3         = getVarInt
  wireGet {- TYPE_SINT64 -} 18       = fmap zzDecode64 getWord64be
  wireGet {- TYPE_SFIXED64 -} 16     = fmap fromIntegral getWord64be

instance Wire Int32 where
  wireSize {- TYPE_INT32 -} 5      x = size'Varint x
  wireSize {- TYPE_SINT32 -} 17    x = size'Varint (zzEncode32 x)
  wireSize {- TYPE_SFIXED32 -} 15  _ = 4
  wirePut {- TYPE_INT32 -} 5       x = putVarSInt x
  wirePut {- TYPE_SINT32 -} 17     x = putVarUInt (zzEncode32 x)
  wirePut {- TYPE_SFIXED32 -} 15   x = putWord32be (fromIntegral x)
  wireGet {- TYPE_INT32 -} 5         = getVarInt
  wireGet {- TYPE_SINT32 -} 17       = fmap zzDecode32 getWord32be
  wireGet {- TYPE_SFIXED32 -} 15     = fmap fromIntegral getWord32be

instance Wire Word64 where
  wireSize {- TYPE_UINT64 -} 4     x = size'Varint x
  wireSize {- TYPE_FIXED64 -} 6    _ = 8
  wirePut {- TYPE_UINT64 -} 4      x = putVarUInt x
  wirePut {- TYPE_FIXED64 -} 6     x = putWord64be x
  wireGet {- TYPE_UINT64 -} 4        = getVarInt
  wireGet {- TYPE_FIXED64 -} 6       = getWord64be

instance Wire Word32 where
  wireSize {- TYPE_UINT32 -} 13    x = size'Varint x
  wireSize {- TYPE_FIXED32 -} 7    _ = 4
  wirePut {- TYPE_UINT32 -} 13     x = putVarUInt x
  wirePut {- TYPE_FIXED32 -} 7     x = putWord32be x
  wireGet {- TYPE_UINT32 -} 13       = getVarInt
  wireGet {- TYPE_FIXED32 -} 7       = getWord32be

instance Wire Bool where
  wireSize {- TYPE_BOOL -} 8       _ = 1
  wirePut {- TYPE_BOOL -} 8    False = putWord8 0
  wirePut {- TYPE_BOOL -} 8    True  = putWord8 1
  wireGet {- TYPE_BOOL -} 8          = do
    x <- getVarInt :: Get Word32 -- google's wire_format_inl.h line 97
    case x of
      0 -> return False
      x | x < 128 -> return True
      _ -> error ("TYPE_BOOL read failure : " ++ show x)

instance Wire ByteString where
  wireSize {- TYPE_STRING -} 9     x = size'Varint len + len where len = BS.length x
  wireSize {- TYPE_BYTES -} 12     x = size'Varint len + len where len = BS.length x
  wirePut {- TYPE_STRING -} 9      x = putVarUInt (BS.length x) >> putLazyByteString x
  wirePut {- TYPE_BYTES -} 12      x = putVarUInt (BS.length x) >> putLazyByteString x
  wireGet {- TYPE_STRING -} 9        = getVarInt >>= getLazyByteString 
  wireGet {- TYPE_BYTES -} 12        = getVarInt >>= getLazyByteString

-- TYPE_GROUP 10
-- TYPE_MESSAGE 11
-- TYPE_ENUM 14
-- -- 

-- This will have to examine the value of positive numbers to get the size
{-# INLINE size'Varint #-}
size'Varint :: (Bits a,Integral a) => a -> Int64
size'Varint b = case compare b 0 of
                  LT -> fromIntegral (divBy (bitSize b) 7)
                  EQ -> 1
                  GT -> genericLength . takeWhile (0<) . iterate (`shiftR` 7) $ b

{-# INLINE divBy #-}
divBy :: (Ord a, Integral a) => a -> a -> a
divBy a b = let (q,r) = quotRem (abs a) b
            in if r==0 then q else succ q

zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral w `shiftR` 1) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral w `shiftR` 1) `xor` (negate (fromIntegral (w .&. 1)))

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

{-# INLINE putVarSInt #-}
putVarSInt :: (Integral a, Bits a) => a -> Put
putVarSInt b =
  case compare b 0 of
    LT -> let len = divBy (bitSize b) 7
              last'Size = (bitSize b)-((pred len)*7)
              last'Mask = pred (1 `shiftL` last'Size)
              go i 1 = putWord8 (fromIntegral i .&. last'Mask)
              go i n = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7) (pred n)
          in go b len
    EQ -> putWord8 0
    GT -> let go i | i < 0x80 = putWord8 (fromIntegral i)
                   | testBit b 7 = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7)
          in go b

{-# INLINE putVarUInt #-}
putVarUInt :: (Integral a, Bits a) => a -> Put
putVarUInt b = let go i | i < 0x80 = putWord8 (fromIntegral i)
                        | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7)
               in go b


{- Useful for testing

testVarInt :: (Integral a, Enum a, Ord a, Bits a) => a -> (Bool,[Word8],Either String a)
testVarInt i = let w = toVarInt i
               in case fromVarInt w of
                    r@(Right v) -> (v==i,w,r)
                    l -> (False,w,l)

fromVarInt :: (Integral a, Bits a) => [Word8] -> Either String a
fromVarInt [] = Left "No bytes!"
fromVarInt (b:bs) = do
  if testBit b 7 then go bs 7 (fromIntegral (b .&. 0x7F))
    else if null bs then Right (fromIntegral b)
           else Left ("Excess bytes: " ++ show (b,bs))
 where
  go [] n val = Left ("Not enough bytes: " ++ show (n,val))
  go (b:bs) n val = do
    if testBit b 7 then go bs (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else if null bs then Right (val .|. ((fromIntegral b) `shiftL` n))
             else Left ("Excess bytes: " ++ show (b,bs,n,val))

toVarInt :: (Integral a, Bits a) => a -> [Word8]
toVarInt b = case compare b 0 of
               LT -> let len = divBy (bitSize b) 7
                         last'Size = (bitSize b) - ((pred len)*7)
                         last'Mask = pred (1 `shiftL` last'Size)
                         go i 1 = [fromIntegral i .&. last'Mask]
                         go i n = (fromIntegral (i .&. 0x7F) .|. 0x80) : go (i `shiftR` 7) (pred n)
                     in go b len
               EQ -> [0]
               GT -> let go i | i < 0x80 = [fromIntegral i]
                              | otherwise = (fromIntegral (i .&. 0x7F) .|. 0x80) : go (i `shiftR` 7)
                     in go b

{-  On my G4 (big endian) powerbook:

le is the protocol-buffer standard (x86 optimized)

*Text.ProtocolBuffers.WireMessage Data.Int Data.Word Numeric> gle . cw $ fle pi
("182d4454fb210940",("word",4614256656552045848,"400921fb54442d18"),("double",3.141592653589793))

be is the network byte order standard (and native for my G4)

*Text.ProtocolBuffers.WireMessage Data.Int Data.Word Numeric> gbe . cw $ fbe pi
("400921fb54442d18",("word",4614256656552045848,"400921fb54442d18"),("double",3.141592653589793))

-}
padL n c s = let l = length s
             in replicate (n-l) c ++ s

cw = concatMap (padL 2 '0')

fbe :: Double -> [String]
fbe (D# d) = let w = W64# (unsafeCoerce# d)
                 b = Build.toLazyByteString (Build.putWord64be w)
             in map (flip showHex "") $  BS.unpack  b

fle :: Double -> [String]
fle (D# d) = let w = W64# (unsafeCoerce# d)
                 b = Build.toLazyByteString (Build.putWord64le w)
             in map (flip showHex "") $ BS.unpack  b

gbe :: [Char] -> ([Char], ([Char], Word64, String), ([Char], Double))
gbe s = let pairs = Data.List.unfoldr (\a -> if Prelude.null a then Nothing
                                               else Just (splitAt 2 a)) s
            words = map (fst . head . readHex) pairs
            w@(W64# w64) = Get.runGet Get.getWord64be (BS.pack words)
            d = D# (unsafeCoerce# w64)
        in (s,("word",w,showHex w ""),("double",d))

gle :: [Char] -> ([Char], ([Char], Word64, String), ([Char], Double))
gle s = let pairs = Data.List.unfoldr (\a -> if Prelude.null a then Nothing
                                               else Just (splitAt 2 a)) s
            words = map (fst . head . readHex) pairs
            w@(W64# w64) = Get.runGet Get.getWord64le (BS.pack words)
            d = D# (unsafeCoerce# w64)
        in (s,("word",w,showHex w ""),("double",d))

-}

newtype FieldId = FieldId { getFieldID :: Word32 } -- really 29 bits, 0 to 2^29-1
  deriving (Eq,Ord,Show,Data,Typeable)

newtype WireId = WireId { getWireID :: Word32 } -- really 3 bits
  deriving (Eq,Ord,Show,Data,Typeable)

data WireData = VarInt ByteString -- the 128 bit variable encoding (least significant first)
              | Fix8 Word32 -- 4 and 8 byte fixed length types, lsb first on wire
              | VarString ByteString -- length of contents as a VarInt
              |           ByteString -- the contents on the wire
              | StartGroup
              | EndGroup
              | Fix4 Word32 
  deriving (Eq,Ord,Show,Data,Typeable)

newtype WireMessage = WireMessage (Map FieldId (Seq WireData))

instance Monoid WireMessage where
  mempty = WireMessage mempty
  mappend (WireMessage a) (WireMessage b) = WireMessage (unionWith mappend a b)

wireId :: WireData -> WireId
wireId (VarInt {})    = WireId 0
wireId (Fix8 {})      = WireId 1
wireId (VarString {}) = WireId 2
wireId  StartGroup    = WireId 3
wireId  StopGroup     = WireId 4
wireId (Fix4 {})      = WireId 5

lookAheadVarIntLength :: Get Int
lookAheadVarIntLength = fmap (either id (error "impossible in lookAheadVarIntLength"))
                             (lookAheadE (go 1))
  where go i = do b <- getWord8
                  if setBit b 7 then go $! succ i else return (Left i)

getWireData = do
  w <- getWord32
  let fieldId = (w `shiftR` 3)
      wireId = w .&. 7
  case wireId of
    0 -> fmap VarInt (getByte

composeFieldWire :: FieldId -> WireId -> Word32
composeFieldWire (FieldId f) (WireId w) = (f `shiftL` 3) .|. w

decomposeFieldWire :: Word32 -> (FieldId,WireId)
decomposeFieldWire x = (FieldId (x `shiftR` 3), WireId (x .&. 7))

encodeWireMessage :: WireMessage -> ByteString
encodeWireMessage = undefined

decodeWireMessage :: ByteString -> WireMessage
decodeWireMessage = undefined

