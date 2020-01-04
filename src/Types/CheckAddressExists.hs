module Types.CheckAddressExists where

import qualified Types.Address as Address
import qualified Types.CheckedAddress as CheckedAddress
import SharedTypes

type CheckAddressExists = Address.Address -> Either ValidationError CheckedAddress.CheckedAddress
