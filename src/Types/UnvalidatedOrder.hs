module Types.UnvalidatedOrder where

import SharedTypes
import Types.UnvalidatedCustomerInfo
import Types.UnvalidatedOrderLine
import Data.List.NonEmpty
import Types.UnvalidatedAddress

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: UnvalidatedCustomerInfo,
  shippingAddress :: UnvalidatedAddress,
  billingAddress :: UnvalidatedAddress,
  orderLines :: [UnvalidatedOrderLine]
}
