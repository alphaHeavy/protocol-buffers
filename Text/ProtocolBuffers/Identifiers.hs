-- | This modules colelct utility routines related to the different
-- incarnations of identifiers in the code.  The basic identifier is
-- always ASCII, but because of the self gereneted DescriptorProto
-- data structures it is stored in 'Utf8' tagged lazy bytestrings.
--
-- An 'identifier' is a non-empty ASCII string made of [a-zA-Z0-9_]
-- where the first cahracter is never in [0-9].
--
-- A 'field' is a mangled identifer that is a valid Haskell name that
-- begins with lower case, and which may have a single quote at the
-- end if needed to avoid a reserved word.
--
-- A 'module' is a mangled identifier that is a valid Haskell name
-- that begins with upper case.  These never have a single quote.  A
-- leading _ is replaced with a leading U'_ to make a valid
-- identifier.
--
-- The flow starts with a Utf8 ByteString from parsing the .proto file,
-- or perhaps from an external source.  These are checked by validDI
-- and promoted to a DIName.
module Text.ProtocolBuffers.Identifiers
  ( unull,toString,fromString
  , IName(..),DIName(..),FIName(..)
  , MName(..),FMName(..),PMName(..)
  , FName(..),FFName(..),PFName(..)
  , Dotted(..),Mangle(..)
  , joinPM,joinPF,difi,splitFI
  , checkDIString,checkDIUtf8
  , promoteFM,promoteFF,dotFM,dotFF,fqAppend
  ) where

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char
import Data.List
import Data.Monoid
import Data.Generics(Data)
import Data.Typeable(Typeable)
import Data.Set(Set)
import qualified Data.Set as S
import Text.ProtocolBuffers.Basic

-- basic utilities to export

unull :: Utf8 -> Bool
unull = LC.null . utf8

toString :: Utf8 -> String
toString = U.toString . utf8

fromString :: String -> Utf8
fromString = Utf8 . U.fromString

-- | Contains one identifier name
newtype IName a = IName {iName::a} deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | Contains one module name, non-empty
newtype MName a = MName {mName::a} deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | Contains one field name, non-empty
newtype FName a = FName {fName::a} deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | '.' separated identifier which may or may start with a dot.  There
-- are never two or more '.'s in a row.  There is always at least one identifier.
newtype DIName a = DIName {diName :: a}
-- | Fully qualified identifier: repeated ('.' then identifier)
newtype FIName a = FIName {fiName::a} deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | Full Haskell module name: MNames separated by '.', ending with a module
newtype FMName a = FMName {fmName::a} deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | Full Haskell field name: MNames separated by '.', ending with a field
newtype FFName a = FFName {ffName::a} deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | Parsed Haskell name ending with MName.  Good contructor to use.
data PMName a = PMName [MName a] (MName a) deriving (Data,Typeable,Show,Read,Eq,Ord)
-- | Parsed Haskell name ending with FName.  Good constructor to use.
data PFName a = PFName [MName a] (FName a) deriving (Data,Typeable,Show,Read,Eq,Ord)

-- | This is used to abstract over Utf8 and String.  The important
-- entry point is 'validDI'.
class (Monoid a) => Dotted a where
  uncons :: a -> Maybe (Char,a)
  cons :: Char -> a -> a
  dot :: a -> a -> a
  -- | 'validDI' ensures the DIName is 
  validDI :: a -> Maybe (DIName a)
  splitDI :: DIName a -> [IName a]

-- These are also part of the external API, they are abstracted over
-- Dotted.

joinPM :: Dotted a => PMName a -> FMName a
joinPM (PMName xs (MName x)) = FMName (foldr dot x . map mName $ xs)

joinPF :: Dotted a => PFName a -> FFName a
joinPF (PFName xs (FName x)) = FFName (foldr dot x . map mName $ xs)

-- | 'difi' examines the 'DIName' and prepend a '.' if absent, promoting
-- it to a 'FIName'.
difi :: Dotted a => DIName a -> FIName a
difi (DIName a) = case uncons a of
                    Just ('.',_) -> FIName a
                    _ -> FIName (cons '.' a)


splitFI :: Dotted a => FIName a -> [IName a]
splitFI = splitDI . DIName . fiName

promoteFM :: Dotted a => MName a -> FMName a
promoteFM = FMName . mName

promoteFF :: Dotted a => FName a -> FFName a
promoteFF = FFName . fName

dotFM :: Dotted a => FMName a -> FMName a -> FMName a
dotFM (FMName a) (FMName b) = FMName (a `dot` b)

dotFF :: Dotted a => FMName a -> FFName a -> FFName a
dotFF (FMName a) (FFName b) = FFName (a `dot` b)

fqAppend :: Dotted a => FIName a -> [IName a] -> FIName a
fqAppend (FIName base) xs = FIName (foldl' dot base . map iName $ xs)

-- The two checkDI* functions give better error messages than validDI

-- | Right (True,_) means the input is a FIName.
-- Right (False,_) means the input is a DIName (without leading '.')
--
-- This creates useful error messages for the user.
checkDIString :: String -> Either String (Bool,[IName String])
checkDIString "" = Left $ "Invalid empty identifier: "++show ""
checkDIString "." = Left $ "Invalid identifier of just a period: "++show "."
checkDIString xs | ('.':ys) <- xs = fmap ((,) True) $ parts id (span ('.'/=) ys)
                 | otherwise = fmap ((,) False) $ parts id (span ('.'/=) xs)
 where parts _f ("","") = Left $ "Invalid identifier because it ends with a period: "++show xs
       parts _f ("",_)  = Left $ "Invalid identifier because is contains two periods in a row: "++show xs
       parts f  (_,"")  = Right (f [])
       parts f  (a,b)   = parts (f . (IName a:)) (span ('.'/=) (tail b))

-- | Right (True,_) means the input is a FIName.
-- Right (False,_) means the input is a DIName (without leading '.')
--
-- This creates useful error messages for the user.
checkDIUtf8 :: Utf8 -> Either String (Bool,[IName Utf8])
checkDIUtf8 s@(Utf8 xs) =
  case U.uncons xs of
    Nothing -> Left $ "Invalid empty identifier: "++show ""
    Just ('.',ys) | LC.null ys -> Left $ "Invalid identifier of just a period: "++show "."
                  | otherwise -> fmap ((,) True) $ parts id (U.span ('.'/=) ys)
    Just _ -> fmap ((,) False) $ parts id (U.span ('.'/=) xs)
 where parts f (a,b) = case (LC.null a,LC.null b) of
                         (True,True) -> Left $ "Invalid identifier because it ends with a period: "++show (toString s)
                         (True,_)    -> Left $ "Invalid identifier because is contains two periods in a row: "++show (toString s)
                         (_,True)    -> Right (f [])
                         _           -> parts (f . (IName (Utf8 a):)) (U.span ('.'/=) (U.drop 1 b))

-- | The 'mangle' transformation has instances for several combiantions
-- of input and output.  These allow one to construct the Haskell types
-- of MName/FMName/PMName and FName/FFName/PFName out of the protobuf
-- types IName/DIName/FIName.  Currently, all the Haskell instances
-- are for the String base type.

class Mangle a b where mangle :: a -> b

instance Mangle (IName String) (MName String) where
  mangle (IName s) = MName (fixUp s)

instance Mangle (IName Utf8) (MName String) where
  mangle (IName s) = MName (fixUp . toString $ s)

instance Mangle (IName String) (FName String) where
  mangle (IName s) = FName (fixLow s)

instance Mangle (IName Utf8) (FName String) where
  mangle (IName s) = FName (fixLow . toString $ s)

instance Mangle (DIName Utf8) (PMName String) where
  mangle s = let ms = splitDI s in PMName (map mangle $ init ms) (mangle $ last ms)

instance Mangle (FIName Utf8) (PMName String) where
  mangle s = let ms = splitFI s in PMName (map mangle $ init ms) (mangle $ last ms)

instance Mangle (DIName Utf8) (PFName String) where
  mangle s = let ms = splitDI s in PFName (map mangle $ init ms) (mangle $ last ms)

instance Mangle (FIName Utf8) (PFName String) where
  mangle s = let ms =  splitFI s in PFName (map mangle $ init ms) (mangle $ last ms)

-- implementation details follow

dotUtf8 :: Utf8 -> Utf8 -> Utf8
dotUtf8 (Utf8 a) (Utf8 b) = Utf8 (LC.append a (LC.cons '.' b))

dotString :: String -> String -> String
dotString a b = a ++ ('.':b)

splitUtf8 :: DIName Utf8 -> [IName Utf8]
splitUtf8 = unfoldr s . utf8 . diName where
  s :: ByteString -> Maybe (IName Utf8,ByteString)
  s y | LC.null y = Nothing
      | otherwise = case U.span ('.'/=) y of
                      (a,b) | LC.null a -> s b
                            | otherwise -> Just (IName (Utf8 a),b)

splitString :: DIName String -> [IName String]
splitString = unfoldr s . diName where
  s [] = Nothing
  s ('.':xs) = s xs -- delete all '.' in the input
  s xs = Just (let (a,b) = span ('.'/=) xs in (IName a,b))

validDIUtf8 :: Utf8 -> Maybe (DIName Utf8)
validDIUtf8 xs | unull xs = Nothing
validDIUtf8 xs@(Utf8 bs) =
  if LC.all (`S.member` validDISet) bs && LC.any ('.'/=) bs
     && (all (\(x,y) -> '.'/=x || '.'/=y) . (\x -> zip (init x) (tail x)) . toString $ xs)
    then Just (DIName xs)
    else Nothing

validDIString :: String -> Maybe (DIName String)
validDIString []  = Nothing
validDIString xs =
  if all (`S.member` validDISet) xs && any ('.'/=) xs
     && all (\(x,y) -> '.'/=x || '.'/=y) (zip (init xs) (tail xs))
    then Just (DIName xs)
    else Nothing

validDISet :: Set Char
validDISet = S.fromDistinctAscList ".ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

instance Dotted Utf8 where
  uncons x = case U.uncons (utf8 x) of
               Nothing -> Nothing
               Just (c,b) -> Just (c,Utf8 b)
  cons b (Utf8 bs) | fromEnum b < 128 = Utf8 (LC.cons b bs)
                   | otherwise = Utf8 ((U.fromString [b]) `mappend` bs)
  dot = dotUtf8
  validDI = validDIUtf8
  splitDI = splitUtf8

instance Dotted String where
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)
  cons = (:)
  dot = dotString
  validDI = validDIString
  splitDI = splitString

err :: String -> a
err s = error ("Text.ProtocolBuffers.ProtoCompile.Identifiers: "++s)

-- make leading upper case letter, and leanding "_" becomes "U'_"
fixUp :: String -> String
fixUp ('_':xs) = "U'"++xs
fixUp i@(x:xs) | isLower x =
  let x' = toUpper x
  in if isLower x' then err ("fixUp: stubborn lower case"++show i)
       else x': xs
fixUp xs = xs

-- make leading '_' or lower case letter, may end with added single quote.
fixLow :: String -> String
fixLow [] = []
fixLow i@(x:xs) | i `S.member` reserved = i ++ "'"
                | isUpper x = let x' = toLower x
                              in if isUpper x' then err ("fixLow: stubborn upper case: "++show i)
                                   else let i' = (x':xs)
                                        in if i' `S.member` reserved then i' ++ "'" else i'
                | otherwise = i

reserved :: Set String
reserved = S.fromDistinctAscList
  ["_"
  ,"case","class","data","default","deriving","do","else","foreign"
  ,"if","import","in","infix","infixl","infixr","instance"
  ,"let","mdo","module","newtype","of","then","type","where"
  ]
