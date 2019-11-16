
module UnvalidatedOrder where

import SharedTypes

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: UnvalidatedAddress
}
