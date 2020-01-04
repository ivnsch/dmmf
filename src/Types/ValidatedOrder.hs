module Types.ValidatedOrder where

import SharedTypes
import Data.List.NonEmpty as NEL
import Types.OrderLine
import Types.OrderId
import Types.CustomerInfo
import Types.CheckedAddress

data ValidatedOrder = ValidatedOrder {
  orderId :: OrderId,
  customerInfo :: CustomerInfo,
  shippingAddress :: CheckedAddress,
  billingAddress :: CheckedAddress,
  orderLines :: [OrderLine]
} deriving (Eq, Show)
