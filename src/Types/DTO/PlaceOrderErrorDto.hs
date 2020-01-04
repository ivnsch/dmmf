module Types.DTO.PlaceOrderErrorDto(PlaceOrderErrorDto, fromDomain) where

import SharedTypes
import Data.Map.Strict
import Types.PlaceOrderEvent

-- ===============================================
--  DTO for PlaceOrderError
-- ===============================================

type PlaceOrderErrorDto = Map String String -- TODO heterogeneous map?

fromDomain :: PlaceOrderError -> PlaceOrderErrorDto
fromDomain (Validation _) =
  undefined -- TODO

fromDomain (Pricing _) =
  undefined -- TODO

fromDomain (RemoteService _) =
  undefined -- TODO
