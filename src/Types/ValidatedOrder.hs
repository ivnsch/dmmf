module Types.ValidatedOrder where

import SharedTypes
import Data.List.NonEmpty as NEL
import Types.OrderLine
import qualified Types.OrderId as OrderId
import qualified Types.CustomerInfo as CustomerInfo
import qualified Types.Address as Address

data ValidatedOrder = ValidatedOrder {
  orderId :: OrderId.OrderId,
  customerInfo :: CustomerInfo.CustomerInfo,
  shippingAddress :: Address.Address,
  billingAddress :: Address.Address,
  orderLines :: NonEmpty OrderLine
} deriving (Eq, Show)
