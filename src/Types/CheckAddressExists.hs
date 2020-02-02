module Types.CheckAddressExists where

import qualified Types.UnvalidatedAddress as UnvalidatedAddress
import qualified Types.CheckedAddress as CheckedAddress
import SharedTypes

type CheckAddressExists = UnvalidatedAddress.UnvalidatedAddress -> Either ValidationError CheckedAddress.CheckedAddress
