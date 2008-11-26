module Text.DescriptorProtos.EnumValueOptions (EnumValueOptions(..)) where
import Prelude ((+), (==), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data EnumValueOptions = EnumValueOptions{uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption,
                                         ext'field :: P'.ExtField, unknown'field :: P'.UnknownField}
                      deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage EnumValueOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage EnumValueOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable EnumValueOptions where
  mergeEmpty = EnumValueOptions P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (EnumValueOptions x'1 x'2 x'3) (EnumValueOptions y'1 y'2 y'3)
   = EnumValueOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default EnumValueOptions where
  defaultValue = EnumValueOptions P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire EnumValueOptions where
  wireSize ft' self'@(EnumValueOptions x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 2 11 x'1 + P'.wireSizeExtField x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(EnumValueOptions x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11
        -> do
             P'.putSize (P'.wireSize 10 self')
             put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 7994 11 x'1
             P'.wirePutExtField x'2
             P'.wirePutUnknownField x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith check'allowed
       11 -> P'.getMessageWith check'allowed
       _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
         = case field'Number of
             999
              -> P'.fmap (\ new'Field -> old'Self{uninterpreted_option = P'.append (uninterpreted_option old'Self) new'Field})
                  (P'.wireGet 11)
             _ -> P'.unknownField old'Self field'Number
        allowed'wire'Tags = P'.fromDistinctAscList [7994]
        check'allowed wire'Tag field'Number wire'Type old'Self
         = P'.catchError
            (if P'.member wire'Tag allowed'wire'Tags then update'Self field'Number old'Self else
              if P'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
               P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self)
            (\ _ -> P'.loadUnknown field'Number wire'Type old'Self)
 
instance P'.MessageAPI msg' (msg' -> EnumValueOptions) EnumValueOptions where
  getVal m' f' = f' m'
 
instance P'.GPB EnumValueOptions
 
instance P'.ReflectDescriptor EnumValueOptions where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [7994])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.EnumValueOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumValueOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumValueOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.EnumValueOptions.uninterpreted_option\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"EnumValueOptions\"], baseName' = FName \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True}"