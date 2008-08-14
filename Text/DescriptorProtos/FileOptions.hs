module Text.DescriptorProtos.FileOptions (FileOptions(..)) where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode
       as DescriptorProtos.FileOptions (OptimizeMode)
 
data FileOptions = FileOptions{java_package ::
                               P'.Maybe P'.ByteString,
                               java_outer_classname :: P'.Maybe P'.ByteString,
                               java_multiple_files :: P'.Maybe P'.Bool,
                               optimize_for :: P'.Maybe DescriptorProtos.FileOptions.OptimizeMode}
                 deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable FileOptions where
        mergeEmpty
          = FileOptions P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
              P'.mergeEmpty
        mergeAppend (FileOptions x'1 x'2 x'3 x'4)
          (FileOptions y'1 y'2 y'3 y'4)
          = FileOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
              (P'.mergeAppend x'3 y'3)
              (P'.mergeAppend x'4 y'4)
 
instance P'.Default FileOptions where
        defaultValue
          = FileOptions (P'.Just P'.defaultValue) (P'.Just P'.defaultValue)
              (P'.Just (P'.False))
              (P'.Just P'.defaultValue)
 
instance P'.Wire FileOptions where
        wireSize 11 (FileOptions x'1 x'2 x'3 x'4)
          = P'.lenSize
              (0 + P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 +
                 P'.wireSizeOpt 1 8 x'3
                 + P'.wireSizeOpt 1 14 x'4)
        wirePut 11 self'@(FileOptions x'1 x'2 x'3 x'4)
          = do P'.putSize (P'.wireSize 11 self')
               P'.wirePutOpt 10 9 x'1
               P'.wirePutOpt 66 9 x'2
               P'.wirePutOpt 80 8 x'3
               P'.wirePutOpt 72 14 x'4
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        1 -> P'.fmap
                               (\ new'Field -> old'Self{java_package = P'.Just new'Field})
                               (P'.wireGet 9)
                        8 -> P'.fmap
                               (\ new'Field -> old'Self{java_outer_classname = P'.Just new'Field})
                               (P'.wireGet 9)
                        10 -> P'.fmap
                                (\ new'Field -> old'Self{java_multiple_files = P'.Just new'Field})
                                (P'.wireGet 8)
                        9 -> P'.fmap
                               (\ new'Field -> old'Self{optimize_for = P'.Just new'Field})
                               (P'.wireGet 14)
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor FileOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FileOptions\"}, fields = fromList [FieldInfo {fieldName = \"java_package\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"java_outer_classname\", fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"java_multiple_files\", fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = \"optimize_for\", fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just \"DescriptorProtos.FileOptions.OptimizeMode\", hsRawDefault = Just (Chunk \"CODE_SIZE\" Empty), hsDefault = Nothing}]}"