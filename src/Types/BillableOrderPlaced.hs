module Types.BillableOrderPlaced where

import Types.OrderId
import Types.CheckedAddress
import Types.BillingAmount

data BillableOrderPlaced = BillableOrderPlaced {
  orderId :: OrderId,
  billingAddress :: CheckedAddress,
  amountToBill :: BillingAmount
}
