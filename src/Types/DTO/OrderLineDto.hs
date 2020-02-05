module Types.DTO.OrderLineDto where

import qualified Types.UnvalidatedOrderLine as UnvalidatedOrderLine

-- ===============================================
--  DTO for OrderLines
-- ===============================================

-- From the order form used as input
data OrderFormLineDto = OrderFormLineDto {
  orderLineId :: String,
  productCode :: String,
  quantity :: Double
} deriving (Eq, Show)

-- Functions relating to the OrderLine DTOs

-- Convert the OrderFormLine into a UnvalidatedOrderLine
-- This always succeeds because there is no validation.
-- Used when importing an OrderForm from the outside world into the domain.
toUnvalidatedOrderLine :: OrderFormLineDto -> UnvalidatedOrderLine.UnvalidatedOrderLine
toUnvalidatedOrderLine dto =
  -- this is a simple 1:1 copy
  UnvalidatedOrderLine.UnvalidatedOrderLine {
    UnvalidatedOrderLine.orderLineId = orderLineId dto,
    UnvalidatedOrderLine.productCode = productCode dto,
    UnvalidatedOrderLine.quantity = quantity dto
  }
