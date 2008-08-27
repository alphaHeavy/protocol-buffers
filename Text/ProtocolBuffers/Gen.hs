-- try "test", "testDesc", and "testLabel" to see sample output
-- 
-- Obsolete : Turn *Proto into Language.Haskell.Exts.Syntax from haskell-src-exts package
-- Now cut back to use just Language.Haskell.Syntax, see coments marked YYY for the Exts verision
-- 
-- Note that this may eventually also generate hs-boot files to allow
-- for breaking mutual recursion.  This is ignored for getting
-- descriptor.proto running.
--
-- Mangling: For the current moment, assume the mangling is done in a prior pass:
--   (*) Uppercase all module names and type names
--   (*) lowercase all field names
--   (*) add a prime after all field names than conflict with reserved words
--
-- The names are also assumed to have become fully-qualified, and all
-- the optional type codes have been set.
--
-- default values are an awful mess.  They are documented in descriptor.proto as
{-
  // For numeric types, contains the original text representation of the value.
  // For booleans, "true" or "false".
  // For strings, contains the default text contents (not escaped in any way).
  // For bytes, contains the C escaped value.  All bytes >= 128 are escaped.
  // TODO(kenton):  Base-64 encode?
  optional string default_value = 7;
-}
module Text.ProtocolBuffers.Gen(descriptorModule,enumModule,prettyPrint) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto) 
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..)) 
import qualified Text.DescriptorProtos.EnumOptions                    as D(EnumOptions)
import qualified Text.DescriptorProtos.EnumOptions                    as D.EnumOptions(EnumOptions(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueOptions               as D(EnumValueOptions) 
import qualified Text.DescriptorProtos.EnumValueOptions               as D.EnumValueOptions(EnumValueOptions(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto(Label)
import           Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto.Label(Label(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto) 
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
{- -- unused or unusable
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions(CType)
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions.CType(CType(..))
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions(OptimizeMode)
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions.OptimizeMode(OptimizeMode(..))
import qualified Text.DescriptorProtos.MessageOptions                 as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions                 as D.MessageOptions(MessageOptions(..))
-}
{-  -- related to the rpc system
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.MethodOptions                  as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions                  as D.MethodOptions(MethodOptions(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto) 
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..)) 
import qualified Text.DescriptorProtos.ServiceOptions                 as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions                 as D.ServiceOptions(ServiceOptions(..))
-}

--import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Default
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage(size'Varint,toWireType,toWireTag)

import qualified Data.ByteString(concat)
import qualified Data.ByteString.Char8(spanEnd)
import qualified Data.ByteString.Lazy.Char8 as BSC(toChunks,fromChunks,length,init,unpack)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString,toString)
import Data.Bits(Bits((.|.),shiftL))
import Data.Maybe(fromMaybe,catMaybes,fromJust)
import Data.List(sort,group,foldl',foldl1',partition)
import Data.Sequence(viewl,ViewL(..),(|>))
import qualified Data.Sequence as Seq(length,fromList,null,empty)
import Data.Foldable as F(foldr,toList)
import Language.Haskell.Syntax
import Language.Haskell.Pretty

-- -- -- -- Helper functions

($$) :: HsExp -> HsExp -> HsExp
($$) = HsApp

infixl 1 $$

dotPre :: String -> String -> String
dotPre "" x = x
dotPre x "" = x
dotPre s x@('.':xs)  | '.' == last s = s ++ xs
                     | otherwise = s ++ x
dotPre s x | '.' == last s = s++x
           | otherwise = s++('.':x)

spanEndL f bs = let (a,b) = Data.ByteString.Char8.spanEnd f (Data.ByteString.concat . BSC.toChunks $ bs)
                in (BSC.fromChunks [a],BSC.fromChunks [b])

-- Take a bytestring of "A" into "Right A" and "A.B.C" into "Left (A.B,C)"
splitMod :: Utf8 -> Either (Utf8,Utf8) Utf8
splitMod (Utf8 bs) = case spanEndL ('.'/=) bs of
                       (pre,post) | BSC.length pre <= 1 -> Right (Utf8 bs)
                                  | otherwise -> Left (Utf8 (BSC.init pre),Utf8 post)

unqual :: Utf8 -> HsQName
unqual bs = UnQual (base bs)

base :: Utf8 -> HsName
base bs = case splitMod bs of
            Right base -> (ident base)
            Left (_,base) -> (ident base)

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

toString = U.toString . utf8

litInt :: Integral x => x -> HsExp
litInt x = HsLit (HsInt (toInteger x))

ident :: Utf8 -> HsName
ident bs = HsIdent (toString bs)

typeApp :: String -> HsType -> HsType
typeApp s =  HsTyApp (HsTyCon (private s))

-- 'qual' and 'qmodname' are only correct for simple or fully looked-up names.
qual :: Utf8 -> HsQName
qual bs = case splitMod bs of
            Right base -> UnQual (ident base)
            Left (parent,base) -> Qual (Module (toString parent)) (ident base)

pvar :: String -> HsExp
pvar t = HsVar (private t)

lvar :: String -> HsExp
lvar t = HsVar (UnQual (HsIdent t))

private :: String -> HsQName
private t = Qual (Module "P'") (HsIdent t)

inst s p r  = HsFunBind [HsMatch src (HsIdent s) p (HsUnGuardedRhs r) noWhere] -- YYY inst s p r  = HsInsDecl (HsFunBind [HsMatch src (HsIdent s) p (HsUnGuardedRhs r) noWhere])

fqName :: ProtoName -> String
fqName (ProtoName a b c) = dotPre a (dotPre b c)

qualName :: ProtoName -> HsQName
qualName (ProtoName prefix "" base) = UnQual (HsIdent base)
qualName (ProtoName prefix parent base) = Qual (Module parent) (HsIdent base)

--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------

data EnumSyntax = EnumSyntax ProtoName [(Integer,HsName)]

toProtoName :: String -> Utf8 -> ProtoName
toProtoName prefix rawName =
  case splitMod rawName of
    Left (m,b) -> ProtoName prefix (toString m) (toString b)
    Right b    -> ProtoName prefix ""           (toString b)

enumModule :: String -> D.EnumDescriptorProto -> HsModule
enumModule prefix
           e@(D.EnumDescriptorProto.EnumDescriptorProto
              { D.EnumDescriptorProto.name = Just rawName})
    = let protoName = toProtoName prefix rawName
          enumSyntax = EnumSyntax protoName (enumValues e)
      in HsModule src (Module (fqName protoName))
                  (Just [HsEThingAll (UnQual (HsIdent (baseName protoName)))])
                  (standardImports False) (enumDecls protoName e)

enumValues :: D.EnumDescriptorProto -> [(Integer,HsName)]
enumValues (D.EnumDescriptorProto.EnumDescriptorProto
            { D.EnumDescriptorProto.value = value}) 
    = sort $ F.foldr ((:) . oneValue) [] value
  where oneValue  :: D.EnumValueDescriptorProto -> (Integer,HsName)
        oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                  { D.EnumValueDescriptorProto.name = Just name
                  , D.EnumValueDescriptorProto.number = Just number })
            = (toInteger number,ident name)
      
enumX :: D.EnumDescriptorProto -> HsDecl
enumX e@(D.EnumDescriptorProto.EnumDescriptorProto
         { D.EnumDescriptorProto.name = Just rawName})
    = HsDataDecl src [] (base rawName) [] (map enumValueX values) derivesEnum --YYY = HsDataDecl src DataType [] (base rawName) [] (map enumValueX values) derivesEnum
        where values = enumValues e
              enumValueX :: (Integer,HsName) -> HsConDecl -- YYY enumValueX :: (Integer,HsName) -> HsQualConDecl
              enumValueX (_,hsName) = HsConDecl src hsName [] -- YYY enumValueX (_,hsName) = HsQualConDecl src [] [] (HsConDecl hsName [])

enumDecls :: ProtoName -> D.EnumDescriptorProto -> [HsDecl]
enumDecls p e = enumX e :  [ instanceMergeableEnum e
                           , instanceBounded e
                           , instanceDefaultEnum e
                           , instanceEnum e
                           , instanceWireEnum e
                           , instanceGPB p 
                           , instanceReflectEnum p e
                           ]

instanceBounded :: D.EnumDescriptorProto -> HsDecl
instanceBounded e@(D.EnumDescriptorProto.EnumDescriptorProto
                   { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Bounded") [HsTyCon (unqual name)] 
                 [set "minBound" (head values),set "maxBound" (last values)]
        where values = enumValues e
              set f (_,n) = inst f [] (HsCon (UnQual n))

instanceEnum :: D.EnumDescriptorProto -> HsDecl
instanceEnum e@(D.EnumDescriptorProto.EnumDescriptorProto
                { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Enum") [HsTyCon (unqual name)] 
                 (map HsFunBind [fromEnum',toEnum',succ',pred']) where -- YYY (map (HsInsDecl . HsFunBind) [fromEnum',toEnum',succ',pred']) where
        values = enumValues e
        fromEnum' = map fromEnum'one values
        fromEnum'one (v,n) = HsMatch src (HsIdent "fromEnum") [HsPApp (UnQual n) []]
                               (HsUnGuardedRhs (HsLit (HsInt v))) noWhere
        toEnum' = map toEnum'one values
        toEnum'one (v,n) = HsMatch src (HsIdent "toEnum") [HsPLit (HsInt v)]
                               (HsUnGuardedRhs (HsCon (UnQual n))) noWhere
        succ' = zipWith (equate "succ") values (tail values)
        pred' = zipWith (equate "pred") (tail values) values
        equate f (_,n1) (_,n2) = HsMatch src (HsIdent f) [HsPApp (UnQual n1) []]
                                   (HsUnGuardedRhs (HsCon (UnQual n2))) noWhere

-- fromEnum TYPE_ENUM == 14 :: Int
instanceWireEnum :: D.EnumDescriptorProto -> HsDecl
instanceWireEnum (D.EnumDescriptorProto.EnumDescriptorProto
                  { D.EnumDescriptorProto.name = Just name })
    = HsInstDecl src [] (private "Wire") [HsTyCon (unqual name)]
      [ withName "wireSize", withName "wirePut", withGet ]
  where withName foo = inst foo [HsPLit (HsInt 14),HsPVar (HsIdent "enum")] rhs
          where rhs = (pvar foo $$ HsLit (HsInt 14)) $$
                      (HsParen $ pvar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [HsPLit (HsInt 14)] rhs
          where rhs = (pvar "fmap" $$ pvar "toEnum") $$
                      (HsParen $ pvar "wireGet" $$ HsLit (HsInt 14))

instanceMergeableEnum :: D.EnumDescriptorProto -> HsDecl
instanceMergeableEnum (D.EnumDescriptorProto.EnumDescriptorProto
                       { D.EnumDescriptorProto.name = Just name }) =
    HsInstDecl src [] (private "Mergeable") [HsTyCon (unqual name)] []

{- from google's descriptor.h, about line 346:

  // Get the field default value if cpp_type() == CPPTYPE_ENUM.  If no
  // explicit default was defined, the default is the first value defined
  // in the enum type (all enum types are required to have at least one value).
  // This never returns NULL.

-}

instanceDefaultEnum :: D.EnumDescriptorProto -> HsDecl
instanceDefaultEnum edp@(D.EnumDescriptorProto.EnumDescriptorProto
                         { D.EnumDescriptorProto.name = Just name
                         , D.EnumDescriptorProto.value = value})
    = HsInstDecl src [] (private "Default") [HsTyCon (unqual name)]
      [ inst "defaultValue" [] firstValue ]
  where firstValue :: HsExp
        firstValue = case viewl value of
                       (:<) (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                             { D.EnumValueDescriptorProto.name = Just name }) _ ->
                                 HsCon (UnQual (ident name))
                       EmptyL -> error $ "EnumDescriptorProto had empty sequence of EnumValueDescriptorProto.\n" ++ show edp

instanceReflectEnum :: ProtoName -> D.EnumDescriptorProto -> HsDecl
instanceReflectEnum protoName@(ProtoName a b c)
                    e@(D.EnumDescriptorProto.EnumDescriptorProto
                       { D.EnumDescriptorProto.name = Just rawName })
    = HsInstDecl src [] (private "ReflectEnum") [HsTyCon (unqual rawName)]
      [ inst "reflectEnum" [] ascList
      , inst "reflectEnumInfo" [ HsPWildCard ] ei
      ]
  where values = enumValues e
        ascList,ei,protoNameExp :: HsExp
        ascList = HsList (map one values)
          where one (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns),HsCon (UnQual n)]
        ei = foldl' HsApp (HsCon (private "EnumInfo")) [protoNameExp,HsList (map two values)]
          where two (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns)]
        protoNameExp = HsParen $ foldl' HsApp (HsCon (private "ProtoName")) [HsLit (HsString a)
                                                                            ,HsLit (HsString b)
                                                                            ,HsLit (HsString c)]

instanceGPB :: ProtoName -> HsDecl
instanceGPB (ProtoName {baseName=name}) =
  HsInstDecl src [] (private "GPB") [HsTyCon (UnQual (HsIdent name))] []

--------------------------------------------
-- DescriptorProto module creation is unfinished
--   There are difficult namespace issues
--------------------------------------------

descriptorModule :: Bool -> String -> D.DescriptorProto -> HsModule
descriptorModule isGroup prefix
                 d@(D.DescriptorProto.DescriptorProto
                    { D.DescriptorProto.name = Just rawName })
    = let self = UnQual . HsIdent . toString . either snd id . splitMod $ rawName
          fqModuleName = Module (dotPre prefix (toString rawName))
          imports = standardImports (isExt d) ++ map formatImport (toImport d)
          protoName = toProtoName prefix rawName
          (insts,di) = instancesDescriptor isGroup protoName d
          exportKeys = map (HsEVar . UnQual . HsIdent . fieldName . snd) (F.toList (keys di))
      in HsModule src fqModuleName (Just (HsEThingAll self : exportKeys)) imports (descriptorX di : (keysX di ++ insts))
  where formatImport (Left (m,t)) = HsImportDecl src (Module (dotPre prefix (dotPre m t))) True
                                      (Just (Module m)) (Just (False,[HsIAbs (HsIdent t)]))
        formatImport (Right t)    = HsImportDecl src (Module (dotPre prefix t)) False
                                      Nothing (Just (False,[HsIAbs (HsIdent t)]))

standardImports ext =
  [ HsImportDecl src (Module "Prelude") False Nothing (Just (False,ops))
  , HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
  , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing ]
 where ops | ext = map (HsIVar . HsSymbol) ["+","<=","&&","||"]
           | otherwise = map (HsIVar . HsSymbol) ["+"]

-- Create a list of (Module,Name) to import
-- Assumes that all self references are _not_ qualified!
toImport :: D.DescriptorProto -> [Either (String,String) String]
toImport msg@(D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field })
    = map head . group . sort
      . map (either (\(m,t) -> Left (toString m,toString t)) (Right . toString))
      . map splitMod
      . filter (selfName /=)
      . concat
      . F.foldr ((:) . mayImport) []
      $ field
  where selfName = either snd id (splitMod name)
        mayImport (D.FieldDescriptorProto.FieldDescriptorProto
                   { D.FieldDescriptorProto.type' = type'
                   , D.FieldDescriptorProto.type_name = type_name
                   , D.FieldDescriptorProto.extendee = maybeExt })
            = answer ++ maybe [] (:[]) maybeExt
          where answer     = maybe answerName checkType type'
                checkType  = maybe answerName (const []) . useType
                answerName = maybe (error $ "No Name for Descriptor!\n" ++ show msg) (:[]) type_name

keysX ::  DescriptorInfo -> [HsDecl]
keysX d = concatMap makeKey . F.toList . keys $ d

makeKey :: KeyInfo -> [HsDecl]
makeKey (extendee,f) = [ keyType, keyVal ]
  where keyType = HsTypeSig src [ HsIdent (fieldName f) ] (HsQualType [] (foldl1 HsTyApp . map HsTyCon $ --YYY  where keyType = HsTypeSig src [ HsIdent (fieldName f) ] (foldl1 HsTyApp . map HsTyCon $
                    [ private "Key", private labeled, qualName extendee, typeQName ]))
        labeled | canRepeat f = "Seq"
                | otherwise = "Maybe"
        typeNumber = getFieldType . typeCode $ f
        typeQName :: HsQName
        typeQName = case useType . toEnum $ typeNumber of
                      Just s -> private s
                      Nothing -> case typeName f of
                                   Just s -> qualName s
                                   Nothing -> error $  "No Name for Field!\n" ++ show f
        keyVal = HsPatBind src (HsPApp (UnQual (HsIdent (fieldName f))) []) (HsUnGuardedRhs
                   (pvar "Key" $$ litInt (getFieldId (fieldNumber f))
                               $$ litInt typeNumber
                               $$ maybe (pvar "Nothing") (HsParen . (pvar "Just" $$) . defToSyntax) (hsDefault f))) noWhere
       
-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
fieldX :: FieldInfo -> ([HsName],HsBangType)
fieldX fi = ([HsIdent (fieldName fi)],HsUnBangedTy (labeled (HsTyCon typed)))
  where labeled | canRepeat fi = typeApp "Seq"
                | isRequired fi = id
                | otherwise = typeApp "Maybe"
        typed :: HsQName
        typed = case useType (toEnum (getFieldType (typeCode fi))) of
                  Just s -> private s
                  Nothing -> case typeName fi of
                               Just s -> qualName s
                               Nothing -> error $  "No Name for Field!\n" ++ show fi

-- HsDataDecl     SrcLoc DataOrNew HsContext HsName [HsName] [HsQualConDecl] [HsQName]
-- data HsQualConDecl = HsQualConDecl SrcLoc {-forall-} [HsTyVarBind] {- . -} HsContext {- => -} HsConDecl
-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
descriptorX :: DescriptorInfo -> HsDecl
descriptorX di = HsDataDecl src [] name [] [con] derives --YYY descriptorX di = HsDataDecl src DataType [] name [] [con] derives
  where name = HsIdent (baseName (descName di))
        con = HsRecDecl src name eFields -- YYY con = HsQualConDecl src [] [] (HsRecDecl name eFields)
                where eFields = F.foldr ((:) . fieldX) end (fields di)
                      end = if hasExt di then [extfield] else []
        extfield :: ([HsName],HsBangType)
        extfield = ([HsIdent "ext'field"],HsUnBangedTy (HsTyCon (Qual (Module "P'") (HsIdent "ExtField"))))

-- There is some confusing code below.  The FieldInfo and
-- DescriptorInfo are getting built as a "side effect" of
-- instanceDefault generating the instances for the Default class.
-- This DescriptorInfo information is then passed to
-- instanceReflectDescriptor to generate the instance of the
-- ReflectDescriptor class.

-- | HsInstDecl     SrcLoc HsContext HsQName [HsType] [HsInstDecl]
instancesDescriptor :: Bool -> ProtoName -> D.DescriptorProto -> ([HsDecl],DescriptorInfo)
instancesDescriptor isGroup protoName d = ([ instanceMergeable di
                                           , instanceDefault di
                                           , instanceWireDescriptor isGroup di
                                           , instanceGPB protoName
                                           , instanceReflectDescriptor di 
                                           ]
                                           ++ if isExt d then [instanceExtendMessage di] else []
                                          ,di)
  where di = makeDescriptorInfo protoName d

isExt :: D.DescriptorProto -> Bool
isExt d = not (Seq.null (D.DescriptorProto.extension_range d))

hasExt :: DescriptorInfo -> Bool
hasExt di = not (null (extRanges di))

instanceExtendMessage :: DescriptorInfo -> HsDecl
instanceExtendMessage di
    = HsInstDecl src [] (private "ExtendMessage") [HsTyCon (UnQual (HsIdent (baseName (descName di))))]
        [ inst "getExtField" [] (lvar "ext'field")
        , inst "putExtField" [HsPVar (HsIdent "e'f"),HsPVar (HsIdent "msg")] putextfield
        , inst "validExtRanges" [ HsPVar (HsIdent "msg") ] (pvar "extRanges" $$ (HsParen $ pvar "reflectDescriptorInfo" $$ lvar "msg"))
        ]
  where putextfield = HsRecUpdate (lvar "msg") [ HsFieldUpdate (UnQual (HsIdent "ext'field")) (lvar "e'f") ]

instanceReflectDescriptor :: DescriptorInfo -> HsDecl
instanceReflectDescriptor di
    = HsInstDecl src [] (private "ReflectDescriptor") [HsTyCon (UnQual (HsIdent (baseName (descName di))))]
        [ inst "reflectDescriptorInfo" [ HsPWildCard ] rdi ]
  where -- massive shortcut through show and read
        rdi :: HsExp
        rdi = pvar "read" $$ HsLit (HsString (show di))

makeDescriptorInfo :: ProtoName -> D.DescriptorProto -> DescriptorInfo
makeDescriptorInfo protoName d@(D.DescriptorProto.DescriptorProto
                                 { D.DescriptorProto.name = Just name
                                 , D.DescriptorProto.field = rawFields
                                 , D.DescriptorProto.extension_range = extension_range })
    = DescriptorInfo protoName fieldInfos keyInfos extRangeList
  where (fields,keys) = partition (\ f -> Nothing == D.FieldDescriptorProto.extendee f) . F.toList $ rawFields
        fieldInfos = Seq.fromList . map toFieldInfo $ fields
        keyInfos = Seq.fromList . map (\f -> (keyNameOf f,toFieldInfo f)) $ keys
        keyNameOf f = case D.FieldDescriptorProto.extendee f of
                        Nothing -> error "Impossible? makeDescriptorInfo partitioned to Just but found Nothing"
                        Just rawName -> toProtoName (haskellPrefix protoName) rawName
        extRangeList = concatMap check unchecked
          where check x@(lo,hi) | hi < lo = []
                                | hi<19000 || 19999<lo  = [x]
                                | otherwise = concatMap check [(lo,18999),(20000,hi)]
                unchecked = F.foldr ((:) . extToPair) [] extension_range
                extToPair (D.DescriptorProto.ExtensionRange
                            { D.DescriptorProto.ExtensionRange.start = start
                            , D.DescriptorProto.ExtensionRange.end = end }) =
                  (maybe minBound FieldId start, maybe maxBound FieldId end)

        toFieldInfo :: D.FieldDescriptorProto -> FieldInfo
        toFieldInfo f@(D.FieldDescriptorProto.FieldDescriptorProto
                        { D.FieldDescriptorProto.name = Just rawName
                        , D.FieldDescriptorProto.number = Just number
                        , D.FieldDescriptorProto.label = Just label
                        , D.FieldDescriptorProto.type' = Just type'
                        , D.FieldDescriptorProto.type_name = mayTypeName
                        , D.FieldDescriptorProto.default_value = mayRawDef })
            = fieldInfo
          where mayDef = parseDefaultValue f
                fieldInfo = let fieldId = (FieldId (fromIntegral number))
                                fieldType = (FieldType (fromEnum type'))
                                wireTag = toWireTag fieldId fieldType
                                wireTagLength = size'Varint (getWireTag wireTag)
                            in FieldInfo (toString rawName)
                                         fieldId
                                         wireTag
                                         wireTagLength
                                         (label == LABEL_REQUIRED)
                                         (label == LABEL_REPEATED)
                                         fieldType
                                         (fmap (toProtoName (haskellPrefix protoName)) mayTypeName)
                                         (fmap utf8 mayRawDef)
                                         mayDef

instanceDefault :: DescriptorInfo -> HsDecl
instanceDefault di
    = HsInstDecl src [] (private "Default") [HsTyCon un]
        [ inst "defaultValue" [] (foldl' HsApp (HsCon un) deflistExt) ]
  where un = UnQual (HsIdent (baseName (descName di)))
        deflistExt = F.foldr ((:) . defX) end (fields di)
        end = if hasExt di then [pvar "defaultValue"] else []

-- "Nothing" means no value specified
-- A failure to parse a provided value will result in an error at the moment
parseDefaultValue :: D.FieldDescriptorProto -> Maybe HsDefault
parseDefaultValue d@(D.FieldDescriptorProto.FieldDescriptorProto
                     { D.FieldDescriptorProto.type' = type'
                     , D.FieldDescriptorProto.default_value = mayDef })
    = do bs <- mayDef
         t <- type'
         todo <- case t of
                   TYPE_MESSAGE -> Nothing
                   TYPE_ENUM    -> Nothing
                   TYPE_GROUP   -> Nothing
                   TYPE_BOOL    -> return parseDefBool
                   TYPE_BYTES   -> return parseDefBytes
                   TYPE_DOUBLE  -> return parseDefDouble
                   TYPE_FLOAT   -> return parseDefFloat
                   TYPE_STRING  -> return parseDefString
                   _            -> return parseDefInteger
         case todo (utf8 bs) of
           Nothing -> error ("Could not parse the default value for "++show d)
           Just value -> return value

defX :: FieldInfo -> HsExp
defX fi | isRequired fi || canRepeat fi = dv
        | otherwise = HsParen $ HsCon (private "Just") $$ dv
  where dv = maybe (pvar "defaultValue") defToSyntax (hsDefault fi)

defToSyntax :: HsDefault -> HsExp
defToSyntax x = HsParen $
  case x of
    HsDef'Bool b -> HsCon (private (show b))
    HsDef'ByteString bs -> pvar "pack" $$ HsLit (HsString (BSC.unpack bs))
    HsDef'Rational r -> HsLit (HsFrac r)
    HsDef'Integer i -> HsLit (HsInt i)

instanceMergeable :: DescriptorInfo -> HsDecl
instanceMergeable di
    = HsInstDecl src [] (private "Mergeable") [HsTyCon un]
        [ inst "mergeEmpty" [] (foldl' HsApp (HsCon un) (replicate len (HsCon (private "mergeEmpty"))))
        , inst "mergeAppend" [HsPApp un patternVars1, HsPApp un patternVars2]
                             (foldl' HsApp (HsCon un) (zipWith append vars1 vars2))
        ]
  where un = UnQual (HsIdent (baseName (descName di)))
        len = (if hasExt di then succ else id) $ Seq.length (fields di)
        patternVars1,patternVars2 :: [HsPat]
        patternVars1 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("x'" ++ show n))) [1..]
        patternVars2 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("y'" ++ show n))) [1..]
        vars1,vars2 :: [HsExp]
        vars1 = take len inf
            where inf = map (\n -> lvar ("x'" ++ show n)) [1..]
        vars2 = take len inf
            where inf = map (\n -> lvar ("y'" ++ show n)) [1..]
        append x y = HsParen $ pvar "mergeAppend" $$ x $$ y

mkOp s a b = HsInfixApp a (HsQVarOp (UnQual (HsSymbol s))) b

instanceWireDescriptor :: Bool -> DescriptorInfo -> HsDecl
instanceWireDescriptor isGroup d@(DescriptorInfo { descName = protoName
                                                 , fields = fieldInfos
                                                 , extRanges = allowedExts })
  = let typeInt = toInteger . fromEnum $ if isGroup then TYPE_GROUP else TYPE_MESSAGE
        myPType = HsPLit (HsInt typeInt)
        myType= HsLit (HsInt typeInt)
        me = UnQual (HsIdent (baseName protoName))
        extensible = not (null allowedExts)
        isAllowed x = pvar "or" $$ HsList ranges where
          (<=!) = mkOp "<="; (&&!) = mkOp "&&"; (||!) = mkOp "||"
          ranges = map (\(FieldId lo,FieldId hi) -> (litInt lo <=! x) &&! (x <=! litInt hi)) allowedExts
        len = (if extensible then succ else id) $ Seq.length fieldInfos
        mine = HsPApp me . take len . map (\n -> HsPVar (HsIdent ("x'" ++ show n))) $ [1..]
        vars = take len . map (\n -> lvar ("x'" ++ show n)) $ [1..]
        (+!) = mkOp "+"
        sizes | null sizesListExt = HsLit (HsInt 0)
              | otherwise = HsParen (foldl1' (+!) sizesListExt)
          where sizesListExt | extensible = sizesList ++ [ pvar "wireSizeExtField" $$ last vars ]
                             | otherwise = sizesList
                sizesList =  zipWith toSize vars . F.toList $ fieldInfos
        toSize var fi = let f = if isRequired fi then "wireSizeReq"
                                  else if canRepeat fi then "wireSizeRep"
                                      else "wireSizeOpt"
                        in foldl' HsApp (pvar f) [ litInt (wireTagLength fi)
                                                 , litInt (getFieldType (typeCode fi))
                                                 , var]
        putMsgSize = HsQualifier $ pvar "putSize" $$
                       (HsParen $ foldl' HsApp (pvar "wireSize") [ myType , lvar "self'" ])
        putStmts = if isGroup then putStmtsContent else putMsgSize:putStmtsContent
          where putStmtsContent | null putStmtsListExt = [HsQualifier $ pvar "return" $$ HsCon (Special HsUnitCon)]
                                | otherwise = putStmtsListExt
                putStmtsListExt | extensible = putStmtsList ++ [ HsQualifier $ pvar "wirePutExtField" $$ last vars ]
                                | otherwise = putStmtsList
                putStmtsList = zipWith toPut vars . F.toList $ fieldInfos
        toPut var fi = let f = if isRequired fi then "wirePutReq"
                                 else if canRepeat fi then "wirePutRep"
                                     else "wirePutOpt"
                       in HsQualifier $
                          foldl' HsApp (pvar f) [ litInt (getWireTag (wireTag fi))
                                                , litInt (getFieldType (typeCode fi))
                                                , var]
        whereUpdateSelf = [HsFunBind [HsMatch src (HsIdent "update'Self") --YYY whereUpdateSelf = HsBDecls [HsFunBind [HsMatch src (HsIdent "update'Self")
                            [HsPVar (HsIdent "field'Number") ,HsPVar (HsIdent "old'Self")]
                            (HsUnGuardedRhs (HsCase (lvar "field'Number") updateAlts)) noWhere]]
        updateAlts = map toUpdate (F.toList fieldInfos) ++ [HsAlt src HsPWildCard (HsUnGuardedAlt $
                       pvar "unknownField" $$ (lvar "field'Number")) noWhere]
        toUpdate fi = HsAlt src (HsPLit . HsInt . toInteger . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $ 
                        pvar "fmap" $$ (HsParen $ HsLambda src [HsPVar (HsIdent "new'Field")] $
                                          HsRecUpdate (lvar "old'Self") [HsFieldUpdate (UnQual . HsIdent . fieldName $ fi)
                                                                                       (labelUpdate fi)])
                                    $$ (HsParen (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdate fi | canRepeat fi = pvar "append" $$ HsParen ((lvar . fieldName $ fi) $$ lvar "old'Self")
                                                      $$ lvar "new'Field"
                       | isRequired fi = qMerge (lvar "new'Field")
                       | otherwise = qMerge (HsCon (private "Just") $$ lvar "new'Field")
            where merges = map fromEnum [ TYPE_MESSAGE, TYPE_GROUP ]
                  qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` merges =
                               pvar "mergeAppend" $$ HsParen ((lvar . fieldName $ fi) $$ lvar "old'Self") $$ (HsParen x)
                           | otherwise = x

    in HsInstDecl src [] (private "Wire") [HsTyCon me]
        [ inst "wireSize" [myPType,mine] sizes
        , inst "wirePut" [myPType,HsPAsPat (HsIdent "self'") (HsPParen mine)] (HsDo putStmts)
        , (HsFunBind [HsMatch src (HsIdent "wireGet") [myPType] (HsUnGuardedRhs $ --YYY , HsInsDecl (HsFunBind [HsMatch src (HsIdent "wireGet") [myPType] (HsUnGuardedRhs $
                                  (pvar (if isGroup then "getBaseMessage"
                                           else if extensible then "getMessageExt"
                                                  else "getMessage") $$
                                        lvar "update'Self")) whereUpdateSelf])
        ]

------------------------------------------------------------------

derives,derivesEnum :: [HsQName]
derives = map private ["Show","Eq","Ord","Typeable"]
derivesEnum = map private ["Read","Show","Eq","Ord","Typeable"]

useType :: Type -> Maybe String
useType TYPE_DOUBLE   = Just "Double"
useType TYPE_FLOAT    = Just "Float"
useType TYPE_BOOL     = Just "Bool"
useType TYPE_STRING   = Just "Utf8"
useType TYPE_BYTES    = Just "ByteString"
useType TYPE_UINT32   = Just "Word32"
useType TYPE_FIXED32  = Just "Word32"
useType TYPE_UINT64   = Just "Word64"
useType TYPE_FIXED64  = Just "Word64"
useType TYPE_INT32    = Just "Int32"
useType TYPE_SINT32   = Just "Int32"
useType TYPE_SFIXED32 = Just "Int32"
useType TYPE_INT64    = Just "Int64"
useType TYPE_SINT64   = Just "Int64"
useType TYPE_SFIXED64 = Just "Int64"
useType TYPE_MESSAGE  = Nothing
useType TYPE_ENUM     = Nothing
useType TYPE_GROUP    = Nothing

noWhere = [] -- YYY noWhere = (HsBDecls [])

test = putStrLn . prettyPrint . descriptorModule False "Text" $ d

testDesc =  putStrLn . prettyPrint . descriptorModule False "Text" $ genFieldOptions

testLabel = putStrLn . prettyPrint $ enumModule "Text" labelTest
testType = putStrLn . prettyPrint $ enumModule "Text" t


-- testing
utf8FromString = Utf8 . U.fromString

-- try and generate a small replacement for my manual file
genFieldOptions :: D.DescriptorProto.DescriptorProto
genFieldOptions =
  defaultValue
  { D.DescriptorProto.name = Just (utf8FromString "DescriptorProtos.FieldOptions") 
  , D.DescriptorProto.field = Seq.fromList
    [ defaultValue
      { D.FieldDescriptorProto.name = Just (utf8FromString "ctype")
      , D.FieldDescriptorProto.number = Just 1
      , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
      , D.FieldDescriptorProto.type' = Just TYPE_ENUM
      , D.FieldDescriptorProto.type_name = Just (utf8FromString "DescriptorProtos.FieldOptions.CType")
      , D.FieldDescriptorProto.default_value = Nothing
      }
    , defaultValue
      { D.FieldDescriptorProto.name = Just (utf8FromString "experimental_map_key")
      , D.FieldDescriptorProto.number = Just 9
      , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
      , D.FieldDescriptorProto.type' = Just TYPE_STRING
      , D.FieldDescriptorProto.default_value = Nothing
      }
    ]
  }

-- test several features
d :: D.DescriptorProto.DescriptorProto
d = defaultValue
    { D.DescriptorProto.name = Just (utf8FromString "SomeMod.ServiceOptions") 
    , D.DescriptorProto.field = Seq.fromList
       [ defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldString")
         , D.FieldDescriptorProto.number = Just 1
         , D.FieldDescriptorProto.label = Just LABEL_REQUIRED
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "Hello World")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldDouble")
         , D.FieldDescriptorProto.number = Just 4
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_DOUBLE
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "+5.5e-10")
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldBytes")
         , D.FieldDescriptorProto.number = Just 2
         , D.FieldDescriptorProto.label = Just LABEL_REQUIRED
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         , D.FieldDescriptorProto.default_value = Just (utf8FromString . map toEnum $ [0,5..255])
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldInt64")
         , D.FieldDescriptorProto.number = Just 3
         , D.FieldDescriptorProto.label = Just LABEL_REQUIRED
         , D.FieldDescriptorProto.type' = Just TYPE_INT64
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "-0x40")
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldBool")
         , D.FieldDescriptorProto.number = Just 5
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "False")
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "field2TestSelf")
         , D.FieldDescriptorProto.number = Just 6
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (utf8FromString "ServiceOptions")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "field3TestQualified")
         , D.FieldDescriptorProto.number = Just 7
         , D.FieldDescriptorProto.label = Just LABEL_REPEATED
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (utf8FromString "A.B.C.Label")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "field4TestUnqualified")
         , D.FieldDescriptorProto.number = Just 8
         , D.FieldDescriptorProto.label = Just LABEL_REPEATED
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (utf8FromString "Maybe")
         }
       ]
    }

labelTest :: D.EnumDescriptorProto.EnumDescriptorProto
labelTest = defaultValue
    { D.EnumDescriptorProto.name = Just (utf8FromString "DescriptorProtos.FieldDescriptorProto.Label")
    , D.EnumDescriptorProto.value = Seq.fromList
      [ defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString "LABEL_OPTIONAL")
                     , D.EnumValueDescriptorProto.number = Just 1 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString "LABEL_REQUIRED")
                     , D.EnumValueDescriptorProto.number = Just 2 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString "LABEL_REPEATED")
                     , D.EnumValueDescriptorProto.number = Just 3 }
      ]
    }

t :: D.EnumDescriptorProto.EnumDescriptorProto
t = defaultValue { D.EnumDescriptorProto.name = Just (utf8FromString "DescriptorProtos.FieldDescriptorProto.Type")
                 , D.EnumDescriptorProto.value = Seq.fromList . zipWith make [1..] $ names }
  where make :: Int32 -> String -> D.EnumValueDescriptorProto
        make i s = defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString s)
                                , D.EnumValueDescriptorProto.number = Just i }
        names = ["TYPE_DOUBLE","TYPE_FLOAT","TYPE_INT64","TYPE_UINT64","TYPE_INT32"
                ,"TYPE_FIXED64","TYPE_FIXED32","TYPE_BOOL","TYPE_STRING","TYPE_GROUP"
                ,"TYPE_MESSAGE","TYPE_BYTES","TYPE_UINT32","TYPE_ENUM","TYPE_SFIXED32"
                ,"TYPE_SFIXED64","TYPE_SINT32","TYPE_SINT64"]
