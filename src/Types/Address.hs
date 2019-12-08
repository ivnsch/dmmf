module Types.Address(
  Address(Address)
) where
  
import SharedTypes
import Types.String50

data Address = Address {
  addressLine1 :: String50,
  addressLine2 :: Maybe String50,
  addressLine3 :: Maybe String50,
  addressLine4 :: Maybe String50,
  city :: City,
  zipCode :: ZipCode
} deriving (Eq, Show)
