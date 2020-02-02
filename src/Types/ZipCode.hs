module Types.ZipCode(
  ZipCode, create, value
) where

import Text.Regex.PCRE
import Text.Printf

newtype ZipCode = ZipCode String deriving (Eq, Show)

-- Return the value inside a ZipCode
value :: ZipCode -> String
value (ZipCode str) = str

-- Create a ZipCode from a string
-- Return Error if input is null, empty, or doesn't have 5 digits
create :: String -> String -> Either String ZipCode
create fieldName str | null str = Left "ZipCode must not be null or empty"
                     | str =~ pattern = Right $ ZipCode str
                     | otherwise = Left $ printf "%s: '%s' must match the pattern '%s'" fieldName str pattern
                     where pattern = "\\d{5}"
