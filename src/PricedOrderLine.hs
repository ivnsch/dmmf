module PricedOrderLine where

import SharedTypes
import OrderId
import OrderLineId
import ProductCode
import Price
import OrderQuantity

data PricedOrderLine = PricedOrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productCode :: ProductCode,
  orderQuantity :: OrderQuantity,
  price :: Price
} deriving (Show)
