module OrderLineId(
  OrderLineId, create, value
) where

newtype OrderLineId = OrderLineId String deriving (Eq, Show)

create :: String -> OrderLineId
create value | null value = error "OrderLineId must not be empty"
             | length value > 50 = error "OrderLineId must not be more than 50 chars"
             | otherwise = OrderLineId value 

value :: OrderLineId -> String
value (OrderLineId str) = str
