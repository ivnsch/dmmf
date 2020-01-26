{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Types.DTO.CustomerInfoDto where

import qualified Types.UnvalidatedCustomerInfo as UnvalidatedCustomerInfo
import GHC.Generics
import Data.Aeson
import qualified Types.String50 as String50
import qualified Types.EmailAddress as EmailAddress
import qualified Types.PersonalName as PersonalName
import qualified Types.CustomerInfo as CustomerInfo

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

-- Convert the DTO into a CustomerInfo object
-- Used when importing from the outside world into the domain, eg loading from a database
toCustomerInfo :: CustomerInfoDto -> Either String CustomerInfo.CustomerInfo
toCustomerInfo dto =
  -- get each (validated) simple type from the DTO as a success or failure
  let
    first = String50.create "FirstName" $ firstName dto
    last = String50.create "LastName" $ lastName dto
    email = EmailAddress.create "EmailAddress" $ emailAddress dto :: Either String EmailAddress.EmailAddress
    -- combine the components to create the domain object
    -- (port) use functor instead?
    name = (,) <$> first <*> last >>=
      \case (f, l) -> Right PersonalName.PersonalName { PersonalName.firstName = f, PersonalName.lastName = l }
    -- (port) use functor instead?
    info = (,) <$> name <*> email >>=
      \case (n, e) -> Right CustomerInfo.CustomerInfo { CustomerInfo.name = n, CustomerInfo.emailAddress = e }
  in
    info

-- Convert a CustomerInfo object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromCustomerInfo :: CustomerInfo.CustomerInfo -> CustomerInfoDto
fromCustomerInfo domainObj =
    -- this is a simple 1:1 copy
  CustomerInfoDto {
    firstName = (String50.value . PersonalName.firstName . CustomerInfo.name) domainObj,
    lastName = (String50.value . PersonalName.lastName . CustomerInfo.name) domainObj,
    emailAddress = (EmailAddress.value . CustomerInfo.emailAddress) domainObj
  }
