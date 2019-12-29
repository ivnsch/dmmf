module Types.CustomerInfo where

import Types.String50
import SharedTypes
import Types.PersonalName
import Types.EmailAddress

data CustomerInfo = CustomerInfo {
  name :: PersonalName,
  emailAddress :: EmailAddress
} deriving (Eq, Show)
