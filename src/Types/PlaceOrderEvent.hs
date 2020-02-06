module Types.PlaceOrderEvent where

import SharedTypes
import Types.PricedOrder
import Types.UnvalidatedOrder
import Types.BillableOrderPlaced
import Types.OrderAcknowledgmentSent

-- TODO better place to put this? in repo it's in PlaceOrder.PublicTypes.fs
type OrderPlaced = PricedOrder

type PlaceOrder = Command UnvalidatedOrder

data PlaceOrderEvent = OrderPlacedEvent OrderPlaced | BillableOrderPlacedEvent BillableOrderPlaced |
  AcknowledgmentSentEvent OrderAcknowledgmentSent

