module Types.CheckedAddress(
  CheckedAddress(CheckedAddress), address
) where
  
import SharedTypes
import Types.UnvalidatedAddress

newtype CheckedAddress = CheckedAddress UnvalidatedAddress deriving (Eq, Show)

address :: CheckedAddress -> UnvalidatedAddress
address (CheckedAddress address) = address
