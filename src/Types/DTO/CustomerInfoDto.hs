{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.CustomerInfoDto where

import qualified Types.UnvalidatedCustomerInfo as UnvalidatedCustomerInfo
import GHC.Generics
import Data.Aeson

-- ===============================================
--  DTO for CustomerInfo
-- ===============================================

data CustomerInfoDto = CustomerInfoDto {
  firstName :: String,
  lastName :: String,
  emailAddress :: String
} deriving (Eq, Show, Generic)

instance FromJSON CustomerInfoDto
instance ToJSON CustomerInfoDto

-- Functions for converting between the DTO and corresponding domain object

-- Convert the DTO into a UnvalidatedCustomerInfo object.
-- This always succeeds because there is no validation.
-- Used when importing an OrderForm from the outside world into the domain.
toUnvalidatedCustomerInfo :: CustomerInfoDto -> UnvalidatedCustomerInfo.UnvalidatedCustomerInfo
toUnvalidatedCustomerInfo dto =
  -- sometimes it's helpful to use an explicit type annotation
  -- to avoid ambiguity between records with the same field names.
  UnvalidatedCustomerInfo.UnvalidatedCustomerInfo {
    -- this is a simple 1:1 copy which always succeeds
    UnvalidatedCustomerInfo.firstName = firstName dto,
    UnvalidatedCustomerInfo.lastName = lastName dto,
    UnvalidatedCustomerInfo.emailAddress = emailAddress dto
  }
