module Types.Address(
  Address(Address), addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode
) where
  
import SharedTypes
import Types.String50

-- TODO rename UnvalidatedAddress
data Address = Address {
  addressLine1 :: String,
  addressLine2 :: String,
  addressLine3 :: String,
  addressLine4 :: String,
  city :: String,
  zipCode :: String
} deriving (Eq, Show)
