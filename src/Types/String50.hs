-- Constrained to be 50 chars or less, not null

module Types.String50(
  String50, create, createOption, value
) where

import Text.Printf

newtype String50 = String50 String deriving (Eq, Show)

maxLen :: Int
maxLen = 50

-- Return the value inside a String50
value :: String50 -> String
value (String50 str) = str

-- Create an String50 from a string
-- Return Error if input is null, empty, or length > 50
create :: String -> String -> Either String String50
create fieldName value | null value = Left $ printf "%s must not be null or empty" fieldName
                       | length value > maxLen = Left $ printf "%s must not be more than %i chars" fieldName maxLen
                       | otherwise = Right $ String50 value

-- Create an String50 from a string
-- Return None if input is null, empty. 
-- Return error if length > maxLen
-- Return Some if the input is valid
-- (port) Either with optional seems weird, confirm with author
-- (port) https://github.com/swlaschin/DomainModelingMadeFunctional/blob/master/src/OrderTaking/Common.SimpleTypes.fs#L86
createOption :: String -> String -> Either String (Maybe String50)
createOption fieldName value | null value = Right Nothing
                             | length value > maxLen = Left $ printf "%s must not be more than %i chars" fieldName maxLen
                             | otherwise = Right $ (Just . String50) value
              