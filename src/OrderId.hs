module OrderId(
  OrderId, create, value
) where

newtype OrderId = OrderId String deriving (Eq, Show)

create :: String -> OrderId
create value | null value = error "OrderId must not be empty"
             | length value > 50 = error "OrderId must not be more than 50 chars"
             | otherwise = OrderId value 

value :: OrderId -> String
value (OrderId str) = str
