
module PlaceOrderWorkflow where

import SharedTypes
import ValidatedOrder
import UnvalidatedOrder
import qualified PricedOrder
import qualified OrderLine

type OrderPlaced = PricedOrder 
type PlaceOrder = Command UnvalidatedOrder

data BillableOrderPlaced = BillableOrderPlaced {
  orderId :: OrderId,
  billingAddress :: BillingAddress,
  amountToBill :: BillingAmount
}

data Order = Unvalidated UnvalidatedOrder | Validated ValidatedOrder | Priced PricedOrder.PricedOrder 

data PlaceOrderEvents = PlaceOrderEvents {
  acknowledgmentSent :: AcknowledgmentSent,
  orderPlaced :: OrderPlaced,
  billableOrderPlaced :: BillableOrderPlaced
}

data PlaceOrderError = ValidationErrors [ValidationError] | NotDefinedYet

-- data PlaceOrderResult = PlaceOrderResult {
--   orderPlaced :: OrderPlaced,
--   billableOrderPlaced :: BillableOrderPlaced,
--   orderAcknowledgmentSent :: OrderAcknowledgmentSent.OrderAcknowledgmentSent 
-- }

-- data PlaceOrderEvent = OrderPlacedEvent OrderPlaced | BillableOrderPlacedEvent BillableOrderPlaced | AcknowledgmentSentEvent AcknowledgmentSent 
