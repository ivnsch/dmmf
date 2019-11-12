module PricedOrderLine where

import SharedTypes

data PricedOrderLine = PricedOrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productId :: ProductId,
  orderQuantity :: Int,
  price :: Price
} deriving (Show)
