module Types.String50(
  String50, string50, value
) where

newtype String50 = String50 String deriving (Eq, Show)

string50 :: String -> String50
string50 value | null value = error "String50 must not be empty" 
               | length value > 50 = error "String50 must not be more than 50 chars"
               | otherwise = String50 value

value :: String50 -> String
value (String50 str) = str
