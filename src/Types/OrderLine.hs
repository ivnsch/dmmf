module Types.OrderLine where

import SharedTypes
import Types.OrderLineId
import Types.OrderQuantity
import Types.ProductCode
import Types.OrderId

data OrderLine = OrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productCode :: ProductCode,
  quantity :: OrderQuantity
} deriving (Eq, Show)
