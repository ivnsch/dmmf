module Types.PdfAttachment where
  
import SharedTypes
import Data.Primitive.ByteArray

-- Represents a PDF attachment
data PdfAttachment = PdfAttachment {
  name :: String,
  bytes :: ByteArray
}
