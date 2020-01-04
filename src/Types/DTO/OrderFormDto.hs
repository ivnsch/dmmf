{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.OrderFormDto where

import Types.DTO.CustomerInfoDto
import Types.DTO.AddressDto
import Types.DTO.OrderFormLineDto
import Prelude hiding (lines)
import qualified Types.UnvalidatedOrder as UnvalidatedOrder
import GHC.Generics
import Data.Aeson

-- ===============================================
--  DTO for OrderForm
-- ===============================================

data OrderFormDto = OrderFormDto {
  orderId :: String,
  customerInfo :: CustomerInfoDto,
  shippingAddress :: AddressDto,
  billingAddress :: AddressDto,
  lines :: [OrderFormLineDto]
} deriving (Eq, Show, Generic)

instance FromJSON OrderFormDto
instance ToJSON OrderFormDto

-- Functions relating to the Order DTOs

-- Convert the OrderForm into a UnvalidatedOrder
-- This always succeeds because there is no validation.
toUnvalidatedOrder :: OrderFormDto -> UnvalidatedOrder.UnvalidatedOrder
toUnvalidatedOrder dto =
  UnvalidatedOrder.UnvalidatedOrder {
    UnvalidatedOrder.orderId = orderId dto,
    UnvalidatedOrder.customerInfo = toUnvalidatedCustomerInfo $ customerInfo dto,
    UnvalidatedOrder.shippingAddress = toUnvalidatedAddress $ shippingAddress dto,
    UnvalidatedOrder.billingAddress = toUnvalidatedAddress $ billingAddress dto,
    UnvalidatedOrder.orderLines = map toUnvalidatedOrderLine $ lines dto
  }
