module CustomerInfo where

import String50
import SharedTypes
import PersonalName

data CustomerInfo = CustomerInfo {
  name :: PersonalName.PersonalName,
  emailAddress :: EmailAddress
} deriving (Eq, Show)
