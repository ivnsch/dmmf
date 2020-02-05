module Types.DTO.OrderLineDto where

import qualified Types.UnvalidatedOrderLine as UnvalidatedOrderLine
import qualified Types.PricedOrderLine as PricedOrderLine
import qualified Types.OrderLineId as OrderLineId
import qualified Types.ProductCode as ProductCode
import qualified Types.OrderQuantity as OrderQuantity
import qualified Types.Price as Price

-- ===============================================
--  DTO for PricedOrderLines
-- ===============================================

-- From the order form used as input
data PricedOrderLineDto = PricedOrderLineDto {
  orderLineId :: String,
  productCode :: String,
  quantity :: Double,
  linePrice :: Double
} deriving (Eq, Show)

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
