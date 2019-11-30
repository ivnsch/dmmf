module OrderDto where

import OrderLineDto

data OrderDto = OrderDto {
  orderId :: String,
  orderLines :: [OrderLineDto]
} deriving (Eq, Show)
