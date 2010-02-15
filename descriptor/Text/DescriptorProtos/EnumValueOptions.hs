module Text.DescriptorProtos.EnumValueOptions (EnumValueOptions(..)) where
import Prelude ((+), (==), (<=), (&&))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data EnumValueOptions = EnumValueOptions{uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption,
                                         ext'field :: P'.ExtField}
                      deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage EnumValueOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable EnumValueOptions where
  mergeEmpty = EnumValueOptions P'.mergeEmpty P'.mergeEmpty
  mergeAppend (EnumValueOptions x'1 x'2) (EnumValueOptions y'1 y'2)
   = EnumValueOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default EnumValueOptions where
  defaultValue = EnumValueOptions P'.defaultValue P'.defaultValue
 
instance P'.Wire EnumValueOptions where
  wireSize ft' self'@(EnumValueOptions x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 2 11 x'1 + P'.wireSizeExtField x'2)
  wirePut ft' self'@(EnumValueOptions x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 7994 11 x'1
             P'.wirePutExtField x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             7994 -> P'.fmap (\ new'Field -> old'Self{uninterpreted_option = P'.append (uninterpreted_option old'Self) new'Field})
                      (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if P'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> EnumValueOptions) EnumValueOptions where
  getVal m' f' = f' m'
 
instance P'.GPB EnumValueOptions
 
instance P'.ReflectDescriptor EnumValueOptions where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [7994])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.EnumValueOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumValueOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumValueOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.EnumValueOptions.uninterpreted_option\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"EnumValueOptions\"], baseName' = FName \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False}"