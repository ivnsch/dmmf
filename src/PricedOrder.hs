module PricedOrder where

import SharedTypes
import PricedOrderLine
import Data.List.NonEmpty
import OrderId
import CustomerInfo
import Address
import BillingAmount

data PricedOrder = PricedOrder {
  orderId :: OrderId,
  customerInfo :: CustomerInfo,
  shippingAddress :: Address,
  billingAddress :: Address,
  orderLines :: NonEmpty PricedOrderLine,
  amountToBill :: BillingAmount
}
