module Types.EmailAddress(
  EmailAddress, create, value
) where

import Text.Regex.PCRE
import Text.Printf

newtype EmailAddress = EmailAddress String deriving (Eq, Show)

-- Return the string value inside an EmailAddress
value :: EmailAddress -> String
value (EmailAddress str) = str

-- Create an EmailAddress from a string
-- Return Error if input is null, empty, or doesn't have an "@" in it
create :: String -> String -> Either String EmailAddress
create fieldName str | null str = Left "Email must not be null or empty"
                     | str =~ pattern = Right $ EmailAddress str
                     | otherwise = Left $ printf "%s: '%s' must match the pattern '%s'" fieldName str pattern
                     where pattern = ".+@.+"
