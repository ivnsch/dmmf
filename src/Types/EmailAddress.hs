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
create :: String -> EmailAddress
create str | null str = error "Email must not be null or empty"
           | str =~ regex = EmailAddress str
           | otherwise = error $ printf "'%s' must match the pattern '%s'" str regex
           where regex = ".+@.+"
