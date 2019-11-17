
module UnvalidatedOrder where

import SharedTypes
import UnvalidatedCustomerInfo
import UnvalidatedOrderLine
import Data.List.NonEmpty

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: UnvalidatedCustomerInfo,
  shippingAddress :: UnvalidatedAddress,
  orderLines :: NonEmpty UnvalidatedOrderLine
}
