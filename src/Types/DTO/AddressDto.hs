{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.AddressDto where

import qualified Types.Address as Address
import GHC.Generics
import Data.Aeson

-- ===============================================
--  DTO for Address
-- ===============================================

data AddressDto = AddressDto {
  addressLine1 :: String,
  addressLine2 :: String,
  addressLine3 :: String,
  addressLine4 :: String,
  city :: String,
  zipCode :: String
} deriving (Eq, Show, Generic)

instance FromJSON AddressDto
instance ToJSON AddressDto

-- Functions for converting between the DTO and corresponding domain object

-- Convert the DTO into a UnvalidatedAddress
-- This always succeeds because there is no validation.
-- Used when importing an OrderForm from the outside world into the domain.
toUnvalidatedAddress :: AddressDto -> Address.Address
toUnvalidatedAddress dto =
  -- this is a simple 1:1 copy
  Address.Address {
    Address.addressLine1 = addressLine1 dto,
    Address.addressLine2 = addressLine2 dto,
    Address.addressLine3 = addressLine3 dto,
    Address.addressLine4 = addressLine4 dto,
    Address.city = city dto,
    Address.zipCode = zipCode dto
  }
