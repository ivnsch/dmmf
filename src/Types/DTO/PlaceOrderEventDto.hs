module Types.DTO.PlaceOrderEventDto(PlaceOrderEventDto, fromDomain) where

import Data.Map.Strict
import Types.PlaceOrderEvent

-- ===============================================
--  DTO for PlaceOrderEvent
-- ===============================================

type PlaceOrderEventDto = Map String String -- TODO heterogeneous map?

fromDomain :: PlaceOrderEvent -> PlaceOrderEventDto
fromDomain (OrderPlacedEvent _) =
  undefined -- TODO

fromDomain (BillableOrderPlacedEvent _) =
  undefined -- TODO

fromDomain (AcknowledgmentSentEvent _) =
  undefined -- TODO
