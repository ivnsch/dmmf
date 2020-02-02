module Types.BillableOrderPlaced where

import Types.OrderId
import Types.Address
import Types.BillingAmount

data BillableOrderPlaced = BillableOrderPlaced {
  orderId :: OrderId,
  billingAddress :: Address,
  amountToBill :: BillingAmount
}
