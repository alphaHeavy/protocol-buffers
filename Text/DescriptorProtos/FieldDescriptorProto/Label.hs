module Text.DescriptorProtos.FieldDescriptorProto.Label (Label(..))
       where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Label = LABEL_OPTIONAL
           | LABEL_REQUIRED
           | LABEL_REPEATED
           deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable Label
 
instance P'.Bounded Label where
        minBound = LABEL_OPTIONAL
        maxBound = LABEL_REPEATED
 
instance P'.Default Label where
        defaultValue = LABEL_OPTIONAL
 
instance P'.Enum Label where
        fromEnum (LABEL_OPTIONAL) = 1
        fromEnum (LABEL_REQUIRED) = 2
        fromEnum (LABEL_REPEATED) = 3
        toEnum 1 = LABEL_OPTIONAL
        toEnum 2 = LABEL_REQUIRED
        toEnum 3 = LABEL_REPEATED
        succ (LABEL_OPTIONAL) = LABEL_REQUIRED
        succ (LABEL_REQUIRED) = LABEL_REPEATED
        pred (LABEL_REQUIRED) = LABEL_OPTIONAL
        pred (LABEL_REPEATED) = LABEL_REQUIRED
 
instance P'.Wire Label where
        wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
        wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
        wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.GPB Label
 
instance P'.ReflectEnum Label where
        reflectEnum
          = [(1, "LABEL_OPTIONAL", LABEL_OPTIONAL),
             (2, "LABEL_REQUIRED", LABEL_REQUIRED),
             (3, "LABEL_REPEATED", LABEL_REPEATED)]
        reflectEnumInfo _
          = P'.EnumInfo
              (P'.ProtoName "Text" "DescriptorProtos.FieldDescriptorProto"
                 "Label")
              [(1, "LABEL_OPTIONAL"), (2, "LABEL_REQUIRED"),
               (3, "LABEL_REPEATED")]