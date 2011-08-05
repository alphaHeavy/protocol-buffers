{-# LANGUAGE MultiParamTypeClasses,TypeSynonymInstances,FlexibleInstances,DeriveDataTypeable #-}
-- | This modules colelct utility routines related to the different
-- incarnations of identifiers in the code.  The basic identifier is
-- always ASCII, but because of the self generated DescriptorProto
-- data structures it is stored in 'Utf8' tagged lazy bytestrings.
--
-- An 'identifier' is a non-empty ASCII string made of [a-zA-Z0-9_]
-- where the first character is never in [0-9].
--
-- A 'field' is a mangled identifer that is a valid Haskell name that
-- begins with lower case, and which may have a single quote at the
-- end if needed to avoid a reserved word.  These may also start with
-- '_', though just a "_" is mangled to "_'".
--
-- A 'module' is a mangled identifier that is a valid Haskell name
-- that begins with upper case.  These never have a single quote.  A
-- leading '_' is replaced with a leading "U'_" to make a valid
-- identifier.
module Text.ProtocolBuffers.Identifiers
  ( IName(..),DIName(..),FIName(..)
  , MName(..),FMName(..),PMName(..)
  , FName(..),FFName(..),PFName(..)
  , Dotted(..),Mangle(..)
  , joinPM,joinPF,difi,splitDI,splitFI,splitFM
  , checkDIString,checkDIUtf8
  , promoteDI,promoteFI,promoteFM,promoteFF,dotFM,dotFF,fqAppend
  ) where

import Data.Char
import Data.List
import Data.Monoid
import Data.Generics(Data)
import Data.Typeable(Typeable)
import Data.Set(Set)
import qualified Data.Set as S
import Text.ProtocolBuffers.Basic
import qualified Data.Text.Lazy as T

-- | Contains one identifier name
newtype IName a = IName {iName::a} deriving (Data,Typeable,Eq,Ord)
-- | Contains one module name, non-empty
newtype MName a = MName {mName::a} deriving (Data,Typeable,Eq,Ord)
-- | Contains one field name, non-empty
newtype FName a = FName {fName::a} deriving (Data,Typeable,Eq,Ord)
-- | '.' separated identifier which may or may start with a dot.  There
-- are never two or more '.'s in a row.  There is always at least one identifier.
newtype DIName a = DIName {diName :: a} deriving (Data,Typeable,Eq,Ord)
-- | Fully qualified identifier: repeated ('.' then identifier)
newtype FIName a = FIName {fiName::a} deriving (Data,Typeable,Eq,Ord)
-- | Full Haskell module name: MNames separated by '.', ending with a module
newtype FMName a = FMName {fmName::a} deriving (Data,Typeable,Eq,Ord)
-- | Full Haskell field name: MNames separated by '.', ending with a field
newtype FFName a = FFName {ffName::a} deriving (Data,Typeable,Eq,Ord)
-- | Parsed Haskell name ending with MName.  Good contructor to use.
data PMName a = PMName [MName a] (MName a) deriving (Show,Data,Typeable,Read,Eq,Ord)
-- | Parsed Haskell name ending with FName.  Good constructor to use.
data PFName a = PFName [MName a] (FName a) deriving (Show,Data,Typeable,Read,Eq,Ord)

app_prec,max_prec :: Int
app_prec = 10
max_prec = 11

{-# INLINE readIt #-}
readIt :: (Read a) => (a -> a1) -> String -> Int -> String -> [(a1, String)]
readIt con name d  = readParen (d > app_prec) (\r -> [(con m,t) | (name',s) <- lex r, name==name', (m,t) <- readsPrec max_prec s])

{-# INLINE showIt #-}
showIt :: (Show a) => Int -> [Char] -> a -> String -> String
showIt d name a = showParen (d > app_prec) $ (name++) . (' ':) . showsPrec max_prec a

instance Read a => Read (IName a) where readsPrec = readIt IName "IName"
instance Read a => Read (MName a) where readsPrec = readIt MName "MName"
instance Read a => Read (FName a) where readsPrec = readIt FName "FName"
instance Read a => Read (DIName a) where readsPrec = readIt DIName "DIName"
instance Read a => Read (FIName a) where readsPrec = readIt FIName "FIName"
instance Read a => Read (FFName a) where readsPrec = readIt FFName "FFName"
instance Read a => Read (FMName a) where readsPrec = readIt FMName "FMName"

instance Show a => Show (IName a) where showsPrec d (IName a) = showIt d "IName" a
instance Show a => Show (MName a) where showsPrec d (MName a) = showIt d "MName" a
instance Show a => Show (FName a) where showsPrec d (FName a) = showIt d "FName" a
instance Show a => Show (DIName a) where showsPrec d (DIName a) = showIt d "DIName" a
instance Show a => Show (FIName a) where showsPrec d (FIName a) = showIt d "FIName" a
instance Show a => Show (FMName a) where showsPrec d (FMName a) = showIt d "FMName" a
instance Show a => Show (FFName a) where showsPrec d (FFName a) = showIt d "FFName" a

-- | This is used to abstract over Utf8 and String.  The important
-- entry point is 'validDI'.
class (Monoid a) => Dotted a where
  uncons :: a -> Maybe (Char,a)
  cons :: Char -> a -> a
  dot :: a -> a -> a
  validI :: a -> Maybe (IName a)
  -- | 'validDI' ensures the DIName is 
  validDI :: a -> Maybe (DIName a)
  -- | 'split' returns a list of non-empty 'a' with all '.' characters removed
  split :: a -> [a]

-- These are also part of the external API, they are abstracted over
-- Dotted.

joinPM :: Dotted a => PMName a -> FMName a
joinPM (PMName xs (MName x)) = FMName (foldr (dot . mName) x xs)

joinPF :: Dotted a => PFName a -> FFName a
joinPF (PFName xs (FName x)) = FFName (foldr (dot . mName) x xs)

-- | 'difi' examines the 'DIName' and prepend a '.' if absent, promoting
-- it to a 'FIName'.
difi :: Dotted a => DIName a -> FIName a
difi (DIName a) = case uncons a of
                    Just ('.',_) -> FIName a
                    _ -> FIName (cons '.' a)

-- | Typed 'split'
splitDI :: Dotted a => DIName a -> [IName a]
splitDI = map IName . split . diName

-- | Typed 'split'
splitFI :: Dotted a => FIName a -> [IName a]
splitFI = map IName . split . fiName

-- | Typed 'split'
splitFM :: Dotted a => FMName a -> [MName a]
splitFM = map MName . split . fmName

promoteDI :: Dotted a => IName a -> DIName a 
promoteDI = DIName . iName

promoteFI :: Dotted a => IName a -> FIName a 
promoteFI = FIName . cons '.' . iName

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
       parts f  (a,"")  = Right (f [IName a])
       parts f  (a,b)   = parts (f . (IName a:)) (span ('.'/=) (tail b))

-- | Right (True,_) means the input is a FIName.
-- Right (False,_) means the input is a DIName (without leading '.')
--
-- This creates useful error messages for the user.
checkDIUtf8 :: T.Text -> Either String (Bool,[IName T.Text])
checkDIUtf8 xs =
  case T.uncons xs of
    Nothing -> Left $ "Invalid empty identifier: "++show ""
    Just ('.',ys) | T.null ys -> Left $ "Invalid identifier of just a period: "++show "."
                  | otherwise -> fmap ((,) True) $ parts id (T.span ('.'/=) ys)
    Just _ -> fmap ((,) False) $ parts id (T.span ('.'/=) xs)
 where parts f (a,b) = case (T.null a,T.null b) of
                         (True,True) -> Left $ "Invalid identifier because it ends with a period: "++show xs
                         (True,_)    -> Left $ "Invalid identifier because is contains two periods in a row: "++show xs
                         (_,True)    -> Right (f [IName a])
                         _           -> parts (f . (IName a:)) (T.span ('.'/=) (T.drop 1 b))
 
-- | The 'mangle' transformation has instances for several combiantions
-- of input and output.  These allow one to construct the Haskell types
-- of MName/FMName/PMName and FName/FFName/PFName out of the protobuf
-- types IName/DIName/FIName.  Currently, all the Haskell instances
-- are for the String base type.

class Mangle a b where mangle :: a -> b

instance Mangle (IName String) (MName String) where
  mangle (IName s) = MName (fixUp s)

instance Mangle (IName T.Text) (MName String) where
  mangle (IName s) = MName (fixUp . T.unpack $ s)

instance Mangle (FName String) (MName String) where
  mangle (FName s) = MName (fixUp s)

instance Mangle (IName String) (FName String) where
  mangle (IName s) = FName (fixLow s)

instance Mangle (IName T.Text) (FName String) where
  mangle (IName s) = FName (fixLow . T.unpack $ s)

instance Mangle (MName String) (FName String) where
  mangle (MName s) = FName (fixLow s)

instance Mangle (DIName T.Text) (PMName String) where
  mangle s = let ms = splitDI s in PMName (map mangle $ init ms) (mangle $ last ms)

instance Mangle (FIName T.Text) (PMName String) where
  mangle s = let ms = splitFI s in PMName (map mangle $ init ms) (mangle $ last ms)

instance Mangle (DIName T.Text) (PFName String) where
  mangle s = let ms = splitDI s in PFName (map mangle $ init ms) (mangle $ last ms)

instance Mangle (FIName T.Text) (PFName String) where
  mangle s = let ms =  splitFI s in PFName (map mangle $ init ms) (mangle $ last ms)

-- implementation details follow

dotUtf8 :: T.Text -> T.Text -> T.Text
dotUtf8 a b = T.append a (T.cons '.' b)

dotString :: String -> String -> String
dotString a b = a ++ ('.':b)

-- | Return list of nonempty Utf8, with all '.' removed
splitUtf8 :: T.Text -> [T.Text]
splitUtf8 = T.split (=='.')

-- | Return list of nonempty String, with all '.' removed
splitString :: String -> [String]
splitString = unfoldr s where
  s [] = Nothing
  s ('.':xs) = s xs -- delete all '.' in the input
  s xs = Just (span ('.'/=) xs)

validIUtf8 :: T.Text -> Maybe (IName T.Text)
validIUtf8 xs | T.null xs = Nothing
validIUtf8 xs = if T.all (`S.member` validISet) xs
                  then Just (IName xs)
                  else Nothing

validIString :: String -> Maybe (IName String)
validIString [] = Nothing
validIString xs = if all (`S.member` validISet) xs
                    then Just (IName xs)
                    else Nothing

validDIUtf8 :: T.Text -> Maybe (DIName T.Text)
validDIUtf8 xs | T.null xs = Nothing
validDIUtf8 xs =
  if T.all (`S.member` validDISet) xs && T.any ('.'/=) xs && T.last xs /= '.'
     && (all (\(x,y) -> '.'/=x || '.'/=y) . (\x -> zip (init x) (tail x)) . T.unpack $ xs)
    then Just (DIName xs)
    else Nothing

validDIString :: String -> Maybe (DIName String)
validDIString []  = Nothing
validDIString xs =
  if all (`S.member` validDISet) xs && any ('.'/=) xs && last xs /= '.'
     && all (\(x,y) -> '.'/=x || '.'/=y) (zip (init xs) (tail xs))
    then Just (DIName xs)
    else Nothing

validISet :: Set Char
validISet = S.fromDistinctAscList "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

validDISet :: Set Char
validDISet = S.fromDistinctAscList ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

instance Dotted Text where
  uncons x = case T.uncons x of
               Nothing -> Nothing
               Just (c,b) -> Just (c,b)
  cons b t | fromEnum b < 128 = T.cons b t
           | otherwise = T.pack [b] `mappend` t
  dot = dotUtf8
  split = splitUtf8
  validI = validIUtf8
  validDI = validDIUtf8

instance Dotted String where
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)
  cons = (:)
  dot = dotString
  split = splitString
  validI = validIString
  validDI = validDIString

err :: String -> a
err s = error ("Text.ProtocolBuffers.ProtoCompile.Identifiers: "++s)

-- make leading upper case letter, and leanding "_" becomes "U'_"
fixUp :: String -> String
fixUp xs | last xs == '\'' = fixUp (init xs) -- in case this is mangling after "fixLow"
fixUp ('_':xs) = "U'"++xs
fixUp i@(x:xs) | isLower x =
  let x' = toUpper x
  in if isLower x' then err ("fixUp: stubborn lower case"++show i)
       else x': xs
fixUp xs = xs

-- make leading '_' or lower case letter, may end with added single quote.
fixLow :: String -> String
fixLow [] = []
fixLow ('U':'\'':xs@('_':_))= fixLow xs
fixLow i@(x:xs) | i `S.member` reserved = i ++ "'"
                | isUpper x = let x' = toLower x
                              in if isUpper x' then err ("fixLow: stubborn upper case: "++show i)
                                   else let i' = (x':xs)
                                        in if i' `S.member` reserved then i' ++ "'" else i'
                | otherwise = i

-- | 'reserved' is a set of strings which are Haskell keywords and
-- should not be valid field names.
--
-- I do not protect these values:
-- "mdo","foreign","rec","proc" ( GHC manual section 8.3.16 )
-- because I do not anticipate using these extensions in the generated
-- Haskell code.
reserved :: Set String
reserved = S.fromDistinctAscList
  ["_"
  ,"case","class","data","default","deriving","do","else"
  ,"forall" {- extension keyword -}
  ,"if","import","in","infix","infixl","infixr","instance"
  ,"let","module","newtype","of","then","type","where"
  ]
