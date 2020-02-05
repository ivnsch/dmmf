module Types.DTO.OrderDto where

import Types.DTO.OrderFormLineDto

data OrderDto = OrderDto {
  orderId :: String,
  orderLines :: [OrderFormLineDto]
} deriving (Eq, Show)
