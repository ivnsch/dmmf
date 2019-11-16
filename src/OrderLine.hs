module OrderLine where

import SharedTypes

data OrderLine = OrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productId :: ProductId,
  orderQuantity :: Int
} deriving (Show)
