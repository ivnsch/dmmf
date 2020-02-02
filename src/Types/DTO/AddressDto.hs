{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.AddressDto where

import qualified Types.Address as Address
import GHC.Generics
import Data.Aeson
import qualified Types.String50 as String50
import qualified Types.ZipCode as ZipCode
import qualified Types.UnvalidatedAddress as UnvalidatedAddress

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
toUnvalidatedAddress :: AddressDto -> UnvalidatedAddress.UnvalidatedAddress
toUnvalidatedAddress dto =
  -- this is a simple 1:1 copy
  UnvalidatedAddress.UnvalidatedAddress {
    UnvalidatedAddress.addressLine1 = addressLine1 dto,
    UnvalidatedAddress.addressLine2 = addressLine2 dto,
    UnvalidatedAddress.addressLine3 = addressLine3 dto,
    UnvalidatedAddress.addressLine4 = addressLine4 dto,
    UnvalidatedAddress.city = city dto,
    UnvalidatedAddress.zipCode = zipCode dto
  }

-- Convert the DTO into a Address object
-- Used when importing from the outside world into the domain, eg loading from a database.
toAddress :: AddressDto -> Either String Address.Address
toAddress dto =
  -- get each (validated) simple type from the DTO as a success or failure
  let
    addressLine1_ = String50.create "AddressLine1" $ addressLine1 dto
    addressLine2_ = String50.createOption "AddressLine2" $ addressLine2 dto
    addressLine3_ = String50.createOption "AddressLine2" $ addressLine2 dto
    addressLine4_ = String50.createOption "AddressLine2" $ addressLine2 dto
    city_ = String50.create "AddressLine2" $ city dto
    zipCode_ = ZipCode.create "AddressLine2" $ zipCode dto
  in
    -- combine the components to create the domain object
    Address.Address <$> addressLine1_ <*> addressLine2_ <*> addressLine3_ <*> addressLine4_ <*> city_ <*> zipCode_

-- Convert a Address object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromAddress :: Address.Address -> AddressDto
fromAddress domainObj =
  -- this is a simple 1:1 copy
  AddressDto {
    addressLine1 = String50.value $ Address.addressLine1 domainObj,
    addressLine2 = maybe "" String50.value $ Address.addressLine2 domainObj,
    addressLine3 = maybe "" String50.value $ Address.addressLine3 domainObj,
    addressLine4 = maybe "" String50.value $ Address.addressLine4 domainObj,
    city = String50.value $ Address.city domainObj,
    zipCode = ZipCode.value $ Address.zipCode domainObj
  }
