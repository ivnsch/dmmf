module Types.CheckedAddress(
  CheckedAddress(CheckedAddress), address
) where
  
import SharedTypes
import Types.Address

newtype CheckedAddress = CheckedAddress Address deriving (Eq, Show)

address :: CheckedAddress -> Address
address (CheckedAddress address) = address
