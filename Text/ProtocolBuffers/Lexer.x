{
module Text.ProtocolBuffers.Lexer (Lexed(..), alexScanTokens,getLinePos)  where

import Control.Monad.Error()
import Codec.Binary.UTF8.String(encode)
import qualified Data.ByteString.Lazy as L
import Data.Char(ord,chr,isHexDigit,isOctDigit,toLower)
import Data.List(sort,unfoldr)
import Data.Word(Word8)
import Numeric(readHex,readOct,readDec,showOct,readSigned,readFloat)

}

%wrapper "posn-bytestring"

@inComment = ([^\*] | $white)+ | ([\*]+ [^\/])
@comment = [\/] [\*] (@inComment)* [\*]+ [\/] | "//".* | "#".*

$d = [0-9]
@decInt = [\-]?[1-9]$d*
@hexInt = [\-]?0[xX]([A-Fa-f0-9])+
@octInt = [\-]?0[0-7]*
@doubleLit = [\-]?$d+(\.$d+)?([Ee][\+\-]?$d+)?

@ident1 = [A-Za-z_][A-Za-z0-9_]*
@ident = [\.]?@ident1([\.]@ident1)*
@notChar = [^A-Za-z0-9_]

@hexEscape = \\[Xx][A-Fa-f0-9]{1,2}
@octEscape = \\0?[0-7]{1,3}
@charEscape = \\[abfnrtv\\\?'\"]
@inStr = @hexEscape | @octEscape | @charEscape | [^'\"\0\n]
@strLit = ['] (@inStr | [\"])* ['] | [\"] (@inStr | ['])* [\"]

$special    = [=\(\)\,\;\[\]\{\}]

:-

  $white+  ;
  @comment ;
  @decInt / @notChar    { parseDec }
  @octInt / @notChar    { parseOct }
  @hexInt / @notChar    { parseHex }
  @doubleLit / @notChar { parseDouble }
  @decInt               { dieAt "decimal followed by invalid character" }
  @octInt               { dieAt "octal followed by invalid character" }
  @hexInt               { dieAt "hex followed by invalid character" }
  @doubleLit            { dieAt "floating followed by invalid character" }
  @strLit               { parseStr }
  @ident                { parseName }
  $special              { parseChar }
  .                     { wtfAt }

{
line :: AlexPosn -> Int
line (AlexPn _byte line' _col) = line'
{-# INLINE line #-}

data Lexed = L_Integer !Int !Integer
           | L_Double !Int !Double
           | L_Name !Int !L.ByteString
           | L_String !Int !L.ByteString
           | L !Int !Char
           | L_Error !Int !String
  deriving (Show,Eq)

getLinePos :: Lexed -> Int
getLinePos x = case x of
                 L_Integer i _ -> i
                 L_Double  i _ -> i
                 L_Name    i _ -> i
                 L_String  i _ -> i
                 L         i _ -> i
                 L_Error   i _ -> i

-- 'errAt' is the only access to L_Error, so I can see where it is created with pos
errAt pos msg =  L_Error (line pos) $ "Lexical error (in Text.ProtocolBuffers.Lexer): "++ msg ++ ", at "++see pos where
  see (AlexPn char line col) = "character "++show char++" line "++show line++" column "++show col++"."
dieAt msg pos _s = errAt pos msg
wtfAt pos s = errAt pos $ "unknown character "++show c++" (decimal "++show (ord c)++")"
  where (c:_) = ByteString.unpack s

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

-- Given the regexps above, the "parse* failed" messages should be impossible.
parseDec pos s = maybe (errAt pos "Impossible? parseDec failed")
                       (L_Integer (line pos)) $ mayRead (readSigned readDec) (ByteString.unpack s)
parseOct pos s = maybe (errAt pos "Impossible? parseOct failed")
                       (L_Integer (line pos)) $ mayRead (readSigned readOct) (ByteString.unpack s)
parseHex pos s = maybe (errAt pos "Impossible? parseHex failed")
                       (L_Integer (line pos)) $ mayRead (readSigned (readHex . drop 2)) (ByteString.unpack s)
parseDouble pos s = maybe (errAt pos "Impossible? parseDouble failed")
                          (L_Double (line pos)) $ mayRead (readSigned readFloat) (ByteString.unpack s)
-- The sDecode of the string contents may fail
parseStr pos s = either (errAt pos) (L_String (line pos) . L.pack) 
               . sDecode . ByteString.unpack
               . ByteString.init . ByteString.tail
               $ s
parseName pos s = L_Name (line pos) s
parseChar pos s = L (line pos) (ByteString.head s)

-- Generalization of concat . unfoldr to monadic-Either form:
op :: ( [Char] -> Either String (Maybe ([Word8],[Char]))) -> [Char] -> Either String [Word8]
op one = go id where
  go f cs = case one cs of
              Left msg -> Left msg
              Right Nothing -> Right (f [])
              Right (Just (ws,cs)) -> go (f . (ws++)) cs

-- Put this mess in the lexer, so the rest of the code can assume
-- everything is saner.  The input is checked to really be "Char8"
-- values in the range [0..255] and to be c-escaped (in order to
-- render binary information printable).  This decodes the c-escaping
-- and returns the binary data as Word8.
-- 
-- A decoding error causes (Left msg) to be returned.
sDecode :: [Char] -> Either String [Word8]
sDecode = op one where
  one :: [Char] -> Either String (Maybe ([Word8],[Char]))
  one (x:xs) | x /= '\\' = do x' <- checkChar8 x
                              return $ Just (x',xs)  -- main case of unescaped value
  one [] = return Nothing
  one ('\\':[]) = Left "cannot understand a string that ends with a backslash"
  one ('\\':ys) | 1 <= len =
      case mayRead readOct oct of
        Just w -> do w' <- checkByte w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode octal sequence "++ys
    where oct = takeWhile isOctDigit (take 3 ys)
          len = length oct
          rest = drop len ys
  one ('\\':x:ys) | 'x' == toLower x && 1 <= len =
      case mayRead readHex hex of
        Just w -> do w' <- checkByte w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode hex sequence "++ys
    where hex = takeWhile isHexDigit (take 2 ys)
          len = length hex
          rest = drop len ys          
  one ('\\':'u':ys) | ok =
      case mayRead readHex hex of
        Just w -> do w' <- checkUnicode w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode 4 char unicode sequence "++ys
    where ok = all isHexDigit hex && 4 == length hex
          (hex,rest) = splitAt 4 ys
  one ('\\':'U':ys) | ok =
      case mayRead readHex hex of
        Just w -> do w' <- checkUnicode w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode 8 char unicode sequence "++ys
    where ok = all isHexDigit hex && 8 == length hex
          (hex,rest) = splitAt 8 ys
  one ('\\':(x:xs)) = do x' <- decode x
                         return $ Just ([x'],xs)
  decode :: Char -> Either String Word8
  decode 'a' = return 7
  decode 'b' = return 8
  decode 't' = return 9
  decode 'n' = return 10
  decode 'v' = return 11
  decode 'f' = return 12
  decode 'r' = return 13
  decode '\"' = return 34
  decode '\'' = return 39
  decode '?' = return 63    -- C99 rule : "\?" is '?'
  decode '\\' = return 92
  decode x | toLower x == 'x' = Left "cannot understand your 'xX' hexadecimal escaped value"
  decode x | toLower x == 'u' = Left "cannot understand your 'uU' unicode UTF-8 hexadecimal escaped value"
  decode _ = Left "cannot understand your backslash-escaped value"
  checkChar8 :: Char -> Either String [Word8]
  checkChar8 c | (0 <= i) && (i <= 255) = Right [toEnum i]
               | otherwise = Left $ "found Char out of range 0..255, value="++show (ord c)
    where i = fromEnum c
  checkByte :: Integer -> Either String [Word8]
  checkByte i | (0 <= i) && (i <= 255) = Right [fromInteger i]
              | otherwise = Left $ "found Oct/Hex Int out of range 0..255, value="++show i
  checkUnicode :: Integer -> Either String [Word8]
  checkUnicode i | (0 <= i) && (i <= 127) = Right [fromInteger i]
                 | i <= maxChar = Right $ encode [ toEnum . fromInteger $ i ]
                 | otherwise = Left $ "found Unicode Char out of range 0..0x10FFFF, value="++show i
    where maxChar = toInteger (fromEnum (maxBound ::Char)) -- 0x10FFFF

}
