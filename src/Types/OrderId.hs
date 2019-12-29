-- An Id for Orders. Constrained to be a non-empty string < 10 chars

module Types.OrderId(
  OrderId, create, value
) where

newtype OrderId = OrderId String deriving (Eq, Show)

create :: String -> Either String OrderId
create value | null value = Left "OrderId must not be empty"
             | length value > 10 = Left "OrderId must not be more than 10 chars"
             | otherwise = Right $ OrderId value 

value :: OrderId -> String
value (OrderId str) = str
