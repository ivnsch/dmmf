module Types.PricedOrder where

import SharedTypes
import Types.PricedOrderLine
import Data.List.NonEmpty
import Types.OrderId
import Types.CustomerInfo
import Types.Address
import Types.BillingAmount

data PricedOrder = PricedOrder {
  orderId :: OrderId,
  customerInfo :: CustomerInfo,
  shippingAddress :: Address,
  billingAddress :: Address,
  orderLines :: NonEmpty PricedOrderLine,
  amountToBill :: BillingAmount
}
