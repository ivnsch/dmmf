module Types.PricedOrderLine where

import SharedTypes
import Types.OrderId
import Types.OrderLineId
import Types.ProductCode
import Types.Price
import Types.OrderQuantity

data PricedOrderLine = PricedOrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productCode :: ProductCode,
  orderQuantity :: OrderQuantity,
  price :: Price
} deriving (Show)
