module Types.UnvalidatedAddress where
  
data UnvalidatedAddress = UnvalidatedAddress {
  addressLine1 :: String,
  addressLine2 :: String,
  addressLine3 :: String,
  addressLine4 :: String,
  city :: String,
  zipCode :: String
} deriving (Eq, Show)
