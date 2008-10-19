module Text.DescriptorProtos.DescriptorProto.ExtensionRange (ExtensionRange(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ExtensionRange = ExtensionRange{start :: P'.Maybe P'.Int32, end :: P'.Maybe P'.Int32, unknown'field :: P'.UnknownField}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage ExtensionRange where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable ExtensionRange where
  mergeEmpty = ExtensionRange P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (ExtensionRange x'1 x'2 x'3) (ExtensionRange y'1 y'2 y'3)
    = ExtensionRange (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default ExtensionRange where
  defaultValue = ExtensionRange P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ExtensionRange where
  wireSize ft' self'@(ExtensionRange x'1 x'2 x'3)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(ExtensionRange x'1 x'2 x'3)
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
              P'.wirePutOpt 8 5 x'1
              P'.wirePutOpt 16 5 x'2
              P'.wirePutUnknownField x'3
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{start = P'.Just new'Field}) (P'.wireGet 5)
              2 -> P'.fmap (\ new'Field -> old'Self{end = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> ExtensionRange) ExtensionRange where
  getVal m' f' = f' m'
 
instance P'.GPB ExtensionRange
 
instance P'.ReflectDescriptor ExtensionRange where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {protobufName = Utf8 {utf8 = Chunk \"MakeReflections.xxx\" Empty}, haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"ExtensionRange\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"DescriptorProto\",\"ExtensionRange.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {protobufName = Utf8 {utf8 = Chunk \"MakeReflections.xxx\" Empty}, haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto.ExtensionRange\", baseName = \"start\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {protobufName = Utf8 {utf8 = Chunk \"MakeReflections.xxx\" Empty}, haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto.ExtensionRange\", baseName = \"end\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"