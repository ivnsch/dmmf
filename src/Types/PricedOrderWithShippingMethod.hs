module Types.PricedOrderWithShippingMethod where

import SharedTypes
import Types.PricedOrder
import Types.ShippingInfo

data PricedOrderWithShippingMethod = PricedOrderWithShippingMethod {
  shippingInfo :: ShippingInfo,
  pricedOrder :: PricedOrder
}
