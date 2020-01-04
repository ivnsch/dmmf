module Types.UnvalidatedOrder where

import SharedTypes
import Types.UnvalidatedCustomerInfo
import Types.UnvalidatedOrderLine
import Data.List.NonEmpty
import Types.Address

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: UnvalidatedCustomerInfo,
  shippingAddress :: Address,
  billingAddress :: Address,
  orderLines :: [UnvalidatedOrderLine]
}
