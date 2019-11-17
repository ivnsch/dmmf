module ValidatedOrder where

import SharedTypes
import Data.List.NonEmpty as NEL
import OrderLine
import qualified OrderId
import qualified CustomerInfo
import qualified Address

data ValidatedOrder = ValidatedOrder {
  orderId :: OrderId.OrderId,
  customerInfo :: CustomerInfo.CustomerInfo,
  shippingAddress :: Address.Address,
  billingAddress :: Address.Address,
  orderLines :: NonEmpty OrderLine
} deriving (Eq, Show)
