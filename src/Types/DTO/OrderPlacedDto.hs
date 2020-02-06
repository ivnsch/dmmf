{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.OrderPlacedDto where

import qualified Types.DTO.CustomerInfoDto as CustomerInfoDto
import qualified Types.DTO.AddressDto as AddressDto
import qualified Types.DTO.OrderFormLineDto as OrderFormLineDto
import qualified Types.DTO.PricedOrderLineDto as PricedOrderLineDto
import Prelude hiding (lines)
import qualified Types.UnvalidatedOrder as UnvalidatedOrder
import GHC.Generics
import Data.Aeson
import qualified Types.PlaceOrderEvent as PlaceOrderEvent
import qualified Types.PricedOrder as PricedOrder
import qualified Types.OrderId as OrderId
import qualified Types.BillingAmount as BillingAmount

-- ===============================================
--  DTO for OrderPlaced event
-- ===============================================

data OrderPlacedDto = OrderPlacedDto {
  orderId :: String,
  customerInfo :: CustomerInfoDto.CustomerInfoDto,
  shippingAddress :: AddressDto.AddressDto,
  billingAddress :: AddressDto.AddressDto,
  amountToBill :: Double,
  lines :: [PricedOrderLineDto.PricedOrderLineDto]
} deriving (Eq, Show, Generic)

instance FromJSON OrderPlacedDto
instance ToJSON OrderPlacedDto

-- Convert a OrderPlaced object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromDomain :: PlaceOrderEvent.OrderPlaced -> OrderPlacedDto
fromDomain domainObj =
  OrderPlacedDto {
    orderId = OrderId.value $ PricedOrder.orderId domainObj,
    customerInfo = CustomerInfoDto.fromCustomerInfo $ PricedOrder.customerInfo domainObj,
    shippingAddress = AddressDto.fromAddress $ PricedOrder.shippingAddress domainObj,
    billingAddress = AddressDto.fromAddress $ PricedOrder.billingAddress domainObj,
    amountToBill = BillingAmount.value $ PricedOrder.amountToBill domainObj,
    lines = map PricedOrderLineDto.fromDomain $ PricedOrder.lines domainObj
  }
