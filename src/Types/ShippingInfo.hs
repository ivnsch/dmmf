module Types.ShippingInfo where

import Types.Price
import Types.ShippingMethod

data ShippingInfo = ShippingInfo {
  shippingMethod :: ShippingMethod,
  shippingCost :: Price
}
