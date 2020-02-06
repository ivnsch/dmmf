{-# LANGUAGE DeriveGeneric #-}

module Types.DTO.OrderAcknowledgmentSentDto where

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
import qualified Types.EmailAddress as EmailAddress
import qualified Types.OrderAcknowledgmentSent as OrderAcknowledgmentSent
import qualified Types.BillingAmount as BillingAmount
import qualified Types.BillableOrderPlaced as BillableOrderPlaced

-- ===============================================
-- DTO for OrderAcknowledgmentSent event
-- ===============================================

-- Event to send to billing context
data OrderAcknowledgmentSentDto = OrderAcknowledgmentSentDto {
  orderId :: String,
  emailAddress :: String
} deriving (Eq, Show, Generic)

instance FromJSON OrderAcknowledgmentSentDto
instance ToJSON OrderAcknowledgmentSentDto

-- Convert a OrderAcknowledgmentSent object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromDomain :: OrderAcknowledgmentSent.OrderAcknowledgmentSent -> OrderAcknowledgmentSentDto
fromDomain domainObj =
  OrderAcknowledgmentSentDto {
    orderId = OrderId.value $ OrderAcknowledgmentSent.orderId domainObj,
    emailAddress = EmailAddress.value $ OrderAcknowledgmentSent.emailAddress domainObj
  }
