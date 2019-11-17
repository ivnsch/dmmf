module OrderLine where

import SharedTypes
import OrderLineId
import OrderQuantity
import ProductCode
import qualified OrderId

data OrderLine = OrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId.OrderId,
  productCode :: ProductCode.ProductCode,
  quantity :: OrderQuantity
} deriving (Eq, Show)
