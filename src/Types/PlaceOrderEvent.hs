module Types.PlaceOrderEvent where

import SharedTypes
import Types.PricedOrder
import Types.UnvalidatedOrder
import Types.BillableOrderPlaced
import Types.OrderAcknowledgmentSent

type OrderPlaced = PricedOrder
type PlaceOrder = Command UnvalidatedOrder

data PlaceOrderEvent = OrderPlacedEvent OrderPlaced | BillableOrderPlacedEvent BillableOrderPlaced |
  AcknowledgmentSentEvent OrderAcknowledgmentSent

