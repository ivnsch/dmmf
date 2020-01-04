module Types.PricedOrder where

import SharedTypes
import Types.PricedOrderLine
import Data.List.NonEmpty
import Types.OrderId
import Types.CustomerInfo
import Types.Address
import Types.BillingAmount
import Types.CheckedAddress

data PricedOrder = PricedOrder {
  orderId :: OrderId,
  customerInfo :: CustomerInfo,
  shippingAddress :: CheckedAddress,
  billingAddress :: CheckedAddress,
  orderLines :: [PricedOrderLine],
  amountToBill :: BillingAmount
}
