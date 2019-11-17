module PersonalName where

import String50

data PersonalName = PersonalName {
  firstName :: String50,
  lastName :: String50
} deriving (Eq, Show)
