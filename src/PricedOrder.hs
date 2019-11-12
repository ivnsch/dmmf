module PricedOrder where

import SharedTypes
import PricedOrderLine
import Data.List.NonEmpty

data PricedOrder = PricedOrder {
  orderId :: OrderId,
  customerInfo :: CustomerInfo,
  shippingAddress :: ShippingAddress,
  billingAddress :: BillingAddress,
  orderLines :: NonEmpty PricedOrderLine,
  amountToBill :: BillingAmount
}
