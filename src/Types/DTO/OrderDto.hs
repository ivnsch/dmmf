module Types.DTO.OrderDto where

import Types.DTO.OrderLineDto

data OrderDto = OrderDto {
  orderId :: String,
  orderLines :: [OrderLineDto]
} deriving (Eq, Show)
