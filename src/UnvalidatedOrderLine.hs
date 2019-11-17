module UnvalidatedOrderLine where

import SharedTypes
import qualified OrderId
import qualified CustomerInfo

data UnvalidatedOrderLine = UnvalidatedOrderLine {
  orderLineId :: String,
  orderId :: String,
  productCode :: String,
  quantity :: Double
}
