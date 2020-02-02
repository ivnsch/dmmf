module Types.PricedOrder where

import SharedTypes
import Types.PricedOrderLine
import Data.List.NonEmpty
import Types.OrderId
import Types.CustomerInfo
import Types.Address
import Types.BillingAmount
import Types.Address

data PricedOrder = PricedOrder {
  orderId :: OrderId,
  customerInfo :: CustomerInfo,
  shippingAddress :: Address,
  billingAddress :: Address,
  orderLines :: [PricedOrderLine],
  amountToBill :: BillingAmount
}
