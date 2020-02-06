{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.PricedOrderLineDto where

import qualified Types.UnvalidatedOrderLine as UnvalidatedOrderLine
import qualified Types.PricedOrderLine as PricedOrderLine
import qualified Types.OrderLineId as OrderLineId
import qualified Types.ProductCode as ProductCode
import qualified Types.OrderQuantity as OrderQuantity
import qualified Types.Price as Price
import GHC.Generics
import Data.Aeson

-- ===============================================
--  DTO for PricedOrderLines
-- ===============================================

-- From the order form used as input
data PricedOrderLineDto = PricedOrderLineDto {
  orderLineId :: String,
  productCode :: String,
  quantity :: Double,
  linePrice :: Double
} deriving (Eq, Show, Generic)

instance FromJSON PricedOrderLineDto
instance ToJSON PricedOrderLineDto

-- Convert a PricedOrderLine object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromDomain :: PricedOrderLine.PricedOrderLine -> PricedOrderLineDto
fromDomain domainObj =
  -- this is a simple 1:1 copy
  PricedOrderLineDto {
    orderLineId = OrderLineId.value $ PricedOrderLine.orderLineId domainObj,
    productCode = ProductCode.value $ PricedOrderLine.productCode domainObj,
    quantity = OrderQuantity.value $ PricedOrderLine.quantity domainObj,
    linePrice =  Price.value $ PricedOrderLine.linePrice domainObj
  }
