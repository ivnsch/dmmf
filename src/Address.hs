module Address(
  Address(Address)
) where
  
import SharedTypes
import String50

data Address = Address {
  addressLine1 :: String50,
  addressLine2 :: String50,
  addressLine3 :: String50,
  addressLine4 :: String50,
  city :: City,
  zipCode :: ZipCode
} deriving (Eq, Show)
