module ValidatedOrder where

import SharedTypes
import Data.List.NonEmpty as NEL
import OrderLine

data ValidatedOrder = ValidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: ShippingAddress,
  billingAddress :: BillingAddress,
  orderLines :: NonEmpty OrderLine
}