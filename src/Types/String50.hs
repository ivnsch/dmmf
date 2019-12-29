-- Constrained to be 50 chars or less, not null

module Types.String50(
  String50, create, createOption, value
) where

newtype String50 = String50 String deriving (Eq, Show)

-- Return the value inside a String50
value :: String50 -> String
value (String50 str) = str

-- Create an String50 from a string
-- Return Error if input is null, empty, or length > 50
create :: String -> String50
create value | null value = error "String50 must not be empty" 
             | length value > 50 = error "String50 must not be more than 50 chars"
             | otherwise = String50 value

-- Create an String50 from a string
-- Return None if input is null, empty. 
-- Return error if length > maxLen
-- Return Some if the input is valid
createOption :: String -> Maybe String50
createOption value | null value = Nothing
                   | length value > 50 = error "String50 must not be more than 50 chars"
                   | otherwise = (Just . String50) value
              