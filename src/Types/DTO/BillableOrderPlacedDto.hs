{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.BillableOrderPlacedDto where

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
import qualified Types.BillableOrderPlaced as BillableOrderPlaced

-- ===============================================
-- DTO for BillableOrderPlaced event
-- ===============================================

-- Event to send to billing context
data BillableOrderPlacedDto = BillableOrderPlacedDto {
  orderId :: String,
  billingAddress :: AddressDto.AddressDto,
  amountToBill :: Double
} deriving (Eq, Show, Generic)

instance FromJSON BillableOrderPlacedDto
instance ToJSON BillableOrderPlacedDto

-- Convert a BillableOrderPlaced object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromDomain :: BillableOrderPlaced.BillableOrderPlaced -> BillableOrderPlacedDto
fromDomain domainObj =
  BillableOrderPlacedDto {
    orderId = OrderId.value $ BillableOrderPlaced.orderId domainObj,
    billingAddress = AddressDto.fromAddress $ BillableOrderPlaced.billingAddress domainObj,
    amountToBill = BillingAmount.value $ BillableOrderPlaced.amountToBill domainObj
  }
