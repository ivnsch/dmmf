module OrderId(
  OrderId, create, value
) where

newtype OrderId = OrderId String deriving (Eq, Show)

create :: String -> Either String OrderId
create value | null value = Left "OrderId must not be empty"
             | length value > 50 = Left "OrderId must not be more than 50 chars"
             | otherwise = Right $ OrderId value 

value :: OrderId -> String
value (OrderId str) = str
