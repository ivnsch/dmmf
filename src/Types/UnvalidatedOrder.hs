module Types.UnvalidatedOrder where

import SharedTypes
import Types.UnvalidatedCustomerInfo
import Types.UnvalidatedOrderLine
import Data.List.NonEmpty

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: UnvalidatedCustomerInfo,
  shippingAddress :: UnvalidatedAddress,
  orderLines :: NonEmpty UnvalidatedOrderLine
}
