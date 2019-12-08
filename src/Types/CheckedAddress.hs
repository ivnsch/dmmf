module Types.CheckedAddress where
  
import SharedTypes

data CheckedAddress = CheckedAddress {
  addressLine1 :: String,
  addressLine2 :: String,
  addressLine3 :: String,
  addressLine4 :: String,
  city :: String,
  zipCode :: String
}
