{-# LANGUAGE OverloadedStrings #-}

module Types.Api.PlaceOrder(
  checkAddressExists, sendOrderAcknowledgment
) where

import qualified Types.Api.HttpRequest as HttpRequest
import qualified Types.Api.HttpResponse as HttpResponse
import qualified Types.PricedOrderWithShippingMethod as PricedOrderWithShippingMethod
import qualified Types.DTO.PlaceOrderEventDto as PlaceOrderEventDto
import qualified Types.DTO.OrderFormDto as OrderFormDto
import Data.Aeson
import qualified Types.DTO.PlaceOrderErrorDto as PlaceOrderErrorDto
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy.Char8 as Char8 (pack, unpack)
import qualified Types.OrderLine as OrderLine
import Prelude hiding (lines, sequence)
import SharedTypes
import qualified Types.ValidatedOrder as ValidatedOrder
import qualified Types.UnvalidatedOrder as UnvalidatedOrder
import qualified Types.OrderId as OrderId
import qualified Types.BillingAmount as BillingAmount
import qualified Types.OrderAcknowledgment as OrderAcknowledgment
import qualified Types.OrderAcknowledgmentSent as OrderAcknowledgmentSent
import qualified Types.CustomerInfo as CustomerInfo
import qualified Types.Address as Address
import qualified Types.OrderLineId as OrderLineId
import qualified Types.UnvalidatedOrderLine as UnvalidatedOrderLine
import qualified Types.CheckedAddress as CheckedAddress
import qualified Types.UnvalidatedCustomerInfo as UnvalidatedCustomerInfo
import qualified Types.PersonalName as PersonalName
import Types.String50 as String50
import qualified Types.ProductCode as ProductCode
import Types.OrderQuantity as OrderQuantity
import qualified Types.UnitQuantity as UnitQuantity
import qualified Types.KilogramQuantity as KilogramQuantity
import qualified Types.PricedOrderLine as POL
import Types.Price as Price
import Control.Arrow(left)
import qualified Types.EmailAddress as EmailAddress
import qualified Types.CheckProductCodeExists as CheckProductCodeExists
import qualified Types.SendOrderAcknowledgment as SendOrderAcknowledgment
import qualified Types.BillableOrderPlaced as BillableOrderPlaced
import qualified Types.PlaceOrderEvent as PlaceOrderEvent
import qualified Types.PricedOrder as PO
import qualified Types.CheckAddressExists as CheckAddressExists
import qualified Data.ByteString.Lazy as L
import qualified Types.UnvalidatedAddress as UnvalidatedAddress
import qualified Types.ZipCode as ZipCode

-- ======================================================
-- This file contains the JSON API interface to the PlaceOrder workflow
--
-- 1) The HttpRequest is turned into a DTO, which is then turned into a Domain object
-- 2) The main workflow function is called
-- 3) The output is turned into a DTO which is turned into a HttpResponse
-- ======================================================

-- An API takes a HttpRequest as input and returns a async response
-- (port) no explicit async needed in Haskell
type PlaceOrderApi = HttpRequest.HttpRequest -> IO HttpResponse.HttpResponse

-- =============================
-- Implementation
-- =============================
--
-- setup dummy dependencies

checkProductExists :: CheckProductCodeExists.CheckProductCodeExists
checkProductExists productCode =
  True -- dummy implementation

-- TODO repo not using Either here, remove?
checkAddressExists :: CheckAddressExists.CheckAddressExists
checkAddressExists unvalidatedAddress = Right $ CheckedAddress.CheckedAddress unvalidatedAddress

type GetProductPrice = ProductCode.ProductCode -> Price.Price

getProductPrice :: GetProductPrice
getProductPrice productCode =
  Price.unsafeCreate 1000000  -- dummy implementation

-- TODO use this instead of CreateAcknowledgmentLetter?
--type CreateOrderAcknowledgmentLetter = PricedOrderWithShippingMethod.PricedOrderWithShippingMethod -> HtmlString
--
--createOrderAcknowledgmentLetter :: CreateOrderAcknowledgmentLetter
--createOrderAcknowledgmentLetter pricedOrder =
--  HtmlString "some text"

sendOrderAcknowledgment :: SendOrderAcknowledgment.SendOrderAcknowledgment
sendOrderAcknowledgment orderAcknowledgement =
  Sent

-- -------------------------------
-- workflow
-- -------------------------------

-- This function converts the workflow output into a HttpResponse
workflowResultToHttpReponse :: Either PlaceOrderError [PlaceOrderEvent.PlaceOrderEvent] -> IO HttpResponse.HttpResponse

workflowResultToHttpReponse (Right events) =
  -- turn domain events into dtos
  let
    dtos = map PlaceOrderEventDto.fromDomain events
    json = encode dtos
  in
    pure $ HttpResponse.HttpResponse 200 $ unpack json

workflowResultToHttpReponse (Left err) =
  -- turn domain errors into a dto
  let
    dto = PlaceOrderErrorDto.fromDomain err
    -- and serialize to JSON
    json = encode dto
    in
      pure $ HttpResponse.HttpResponse 401 $ unpack json

placeOrderApi :: PlaceOrderApi
placeOrderApi request =
  -- following the approach in "A Complete Serialization Pipeline" in chapter 11
  -- start with a string
  let
    orderFormJson = HttpRequest.body request
    orderForm = unsafeUnwrap (decode (pack orderFormJson) :: Maybe OrderFormDto.OrderFormDto)

    -- convert to domain object
    unvalidatedOrder = OrderFormDto.toUnvalidatedOrder orderForm

    -- setup the dependencies. See "Injecting Dependencies" in chapter 9
    workflow =
       placeOrder
         checkProductExists -- dependency
         checkAddressExists -- dependency
         getProductPrice -- dependency
         createAcknowledgmentLetter  -- dependency
         sendOrderAcknowledgment -- dependency

    -- now we are in the pure domain
    result = workflow unvalidatedOrder

  in
    -- now convert from the pure domain back to a HttpResponse
    workflowResultToHttpReponse result

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-- TODO separation api / implementation?
-- TODO computation expressions -> do notation? Go through usages of >>= and see if it's worth replacing with do

placeOrder :: CheckProductCodeExists.CheckProductCodeExists -> CheckAddressExists.CheckAddressExists ->
  GetProductPrice -> CreateAcknowledgmentLetter -> SendOrderAcknowledgment.SendOrderAcknowledgment ->
  UnvalidatedOrder.UnvalidatedOrder -> Either PlaceOrderError [PlaceOrderEvent.PlaceOrderEvent]
placeOrder checkProductCodeExists  checkAddressExists getProductPrice createAcknowledgmentLetter sendOrderAcknowledgment unvalidatedOrder =
  let
    validateOrder' = validateOrderAdapted checkProductCodeExists checkAddressExists
    priceOrder' = priceOrderAdapted getProductPrice
    acknowledgeOrder' = acknowledgeOrder createAcknowledgmentLetter sendOrderAcknowledgment

    validatedOrder = validateOrder' unvalidatedOrder
    pricedOrder = validatedOrder >>= priceOrder'
    acknowledmentOption = acknowledgeOrder' <$> pricedOrder
  in
    createEvents <$> pricedOrder <*> acknowledmentOption

validateOrder :: CheckProductCodeExists.CheckProductCodeExists -> CheckAddressExists.CheckAddressExists ->
  UnvalidatedOrder.UnvalidatedOrder -> Either ValidationError ValidatedOrder.ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder =
  let
    orderId = (OrderId.create . UnvalidatedOrder.orderId) unvalidatedOrder
    orderIdWithLeftValidationError = left (ValidationError "OrderId") orderId -- TODO compare with repo
    customerInfo = (toCustomerInfo . UnvalidatedOrder.customerInfo) unvalidatedOrder
    shippingAddress = checkAddressExists (UnvalidatedOrder.shippingAddress unvalidatedOrder) >>= toAddress
    billingAddress = checkAddressExists (UnvalidatedOrder.billingAddress unvalidatedOrder) >>= toAddress
    orderLines = (map (toValidatedOrderLine checkProductCodeExists) . UnvalidatedOrder.orderLines) unvalidatedOrder
  in
    ValidatedOrder.ValidatedOrder <$> orderIdWithLeftValidationError <*> customerInfo <*> shippingAddress <*> billingAddress <*> sequence orderLines

validateOrderAdapted :: CheckProductCodeExists.CheckProductCodeExists -> CheckAddressExists.CheckAddressExists -> UnvalidatedOrder.UnvalidatedOrder -> Either PlaceOrderError ValidatedOrder.ValidatedOrder
validateOrderAdapted checkProductCodeExists checkAddressExists unvalidatedOrder =
  left Validation $ validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder

toCustomerInfo :: UnvalidatedCustomerInfo.UnvalidatedCustomerInfo -> Either ValidationError CustomerInfo.CustomerInfo
toCustomerInfo unvalidatedCustomerInfo =
  -- TODO check which field names repo uses here
  let firstName = (String50.create "firstName" . UnvalidatedCustomerInfo.firstName) unvalidatedCustomerInfo
      lastName = (String50.create "lastName" . UnvalidatedCustomerInfo.lastName) unvalidatedCustomerInfo
      emailAddress = (EmailAddress.create "emailAddress" . UnvalidatedCustomerInfo.emailAddress) unvalidatedCustomerInfo
      name = PersonalName.PersonalName <$> firstName <*> lastName
  -- TODO check how repo does this
  in left (ValidationError "") $ CustomerInfo.CustomerInfo <$> name <*> emailAddress

toAddress :: CheckedAddress.CheckedAddress -> Either ValidationError Address.Address
toAddress checkedAddress =
  let
    unvalidatedAddress = CheckedAddress.address checkedAddress
    addressLine1 = left (ValidationError "") $ String50.create "addressLine1" $ UnvalidatedAddress.addressLine1 unvalidatedAddress
    addressLine2 = left (ValidationError "") $ String50.createOption "addressLine2" $ UnvalidatedAddress.addressLine1 unvalidatedAddress
    addressLine3 = left (ValidationError "") $ String50.createOption "addressLine3" $ UnvalidatedAddress.addressLine1 unvalidatedAddress
    addressLine4 = left (ValidationError "") $ String50.createOption "addressLine4" $ UnvalidatedAddress.addressLine1 unvalidatedAddress
    city = left (ValidationError "") $ String50.create "city" $ UnvalidatedAddress.city unvalidatedAddress
    zipCode = left (ValidationError "") $ ZipCode.create "zipCode" $ UnvalidatedAddress.zipCode unvalidatedAddress
  in
    Address.Address <$> addressLine1 <*> addressLine2 <*> addressLine3 <*> addressLine4 <*> city <*> zipCode

checkProductCodeExists :: CheckProductCodeExists.CheckProductCodeExists
checkProductCodeExists productCode = undefined

priceOrder :: GetProductPrice -> ValidatedOrder.ValidatedOrder -> Either PricingError PO.PricedOrder
priceOrder getProductPrice validatedOrder =
  let
    lines = (map (toPricedOrderLine getProductPrice) . ValidatedOrder.orderLines) validatedOrder
    sumPrice = (sumPrices . map POL.price) lines
    amountToBill = BillingAmount.BillingAmount $ case sumPrice of Price value -> value
  in
    Right $ PO.PricedOrder
      (ValidatedOrder.orderId validatedOrder)
      (ValidatedOrder.customerInfo validatedOrder)
      (ValidatedOrder.shippingAddress validatedOrder)
      (ValidatedOrder.billingAddress validatedOrder)
      lines
      amountToBill

priceOrderAdapted :: GetProductPrice -> ValidatedOrder.ValidatedOrder -> Either PlaceOrderError PO.PricedOrder
priceOrderAdapted getProductPrice validatedOrder =
  left Pricing $ priceOrder getProductPrice validatedOrder

toPricedOrderLine :: GetProductPrice -> OrderLine.OrderLine -> POL.PricedOrderLine
toPricedOrderLine getProductPrice line =
  let
    qty = (OrderQuantity.value . OrderLine.quantity) line
    price = (getProductPrice . OrderLine.productCode) line
    linePrice = Price.multiply qty price
  in
    POL.PricedOrderLine
      (OrderLine.orderLineId line)
      (OrderLine.orderId line)
      (OrderLine.productCode line)
      (OrderLine.quantity line)
      linePrice

sumPrices :: [Price] -> Price
sumPrices prices = Price $ foldr (\p a -> case p of Price value  -> value + a) 0 prices

-- type CreateEvents = PO.PricedOrder -> [PlaceOrderEvent]
createEvents :: PO.PricedOrder -> Maybe OrderAcknowledgmentSent.OrderAcknowledgmentSent -> [PlaceOrderEvent.PlaceOrderEvent]
createEvents pricedOrder acknowledgmentEventOpt =
  let
    events1 = [PlaceOrderEvent.OrderPlacedEvent pricedOrder]
    events2 = listOfOpt $ PlaceOrderEvent.AcknowledgmentSentEvent <$> acknowledgmentEventOpt
    events3 = listOfOpt $ PlaceOrderEvent.BillableOrderPlacedEvent <$> createBillingEvent pricedOrder
  in
    events1 <> events2 <> events3

data Order = Unvalidated UnvalidatedOrder.UnvalidatedOrder | Validated ValidatedOrder.ValidatedOrder | Priced PO.PricedOrder

data PlaceOrderEvents = PlaceOrderEvents {
  acknowledgmentSent :: OrderAcknowledgment.OrderAcknowledgment,
  orderPlaced :: PlaceOrderEvent.OrderPlaced,
  billableOrderPlaced :: BillableOrderPlaced.BillableOrderPlaced
}


-- data PlaceOrderResult = PlaceOrderResult {
--   orderPlaced :: OrderPlaced,
--   billableOrderPlaced :: BillableOrderPlaced,
--   orderAcknowledgmentSent :: OrderAcknowledgmentSent.OrderAcknowledgmentSent
-- }

type CreateAcknowledgmentLetter = PO.PricedOrder -> HtmlString
createAcknowledgmentLetter :: CreateAcknowledgmentLetter
createAcknowledgmentLetter = undefined

acknowledgeOrder :: CreateAcknowledgmentLetter -> SendOrderAcknowledgment.SendOrderAcknowledgment -> PO.PricedOrder ->
  Maybe OrderAcknowledgmentSent.OrderAcknowledgmentSent
acknowledgeOrder createAcknowledgmentLetter sendAcknowledgment pricedOrder =
  let
    letter = createAcknowledgmentLetter pricedOrder
    acknowledgment = OrderAcknowledgment.OrderAcknowledgment (CustomerInfo.emailAddress $ PO.customerInfo pricedOrder) letter
  in
    case sendAcknowledgment acknowledgment of
      Sent -> Just $ OrderAcknowledgmentSent.OrderAcknowledgmentSent (PO.orderId pricedOrder) (CustomerInfo.emailAddress $ PO.customerInfo pricedOrder)
      NotSent -> Nothing

listOfOpt :: Maybe a -> [a]
listOfOpt (Just value) = [value]
listOfOpt Nothing = []


toProductCode :: CheckProductCodeExists.CheckProductCodeExists -> String -> ProductCode.ProductCode
toProductCode checkProductCodeExists str =
  let
    errorMsg = "Invalid: " <> str
    result = predicatePassthru errorMsg checkProductCodeExists str
  in
    ProductCode.create result

predicatePassthru :: String -> (a -> Bool) -> a -> a
predicatePassthru errorMsg f x = if f x then x else error errorMsg

toOrderQuantity :: ProductCode.ProductCode -> Double -> OrderQuantity
toOrderQuantity productCode value =
  case productCode of
    ProductCode.Widget _ -> (UnitQuantity . UnitQuantity.create) (round value :: Int)
    ProductCode.Gizmo _ -> (KilogramQuantity . KilogramQuantity.create) value

toValidatedOrderLine :: CheckProductCodeExists.CheckProductCodeExists -> UnvalidatedOrderLine.UnvalidatedOrderLine -> Either ValidationError OrderLine.OrderLine
toValidatedOrderLine checkProductCodeExists unvalidatedOrderLine =
  let
    orderLineId = (OrderLineId.create . UnvalidatedOrderLine.orderLineId) unvalidatedOrderLine
--    orderId = (OrderId.create . UnvalidatedOrderLine.orderId) unvalidatedOrderLine
    orderId = OrderId.create "123" -- TODO removed order id UnvalidatedOrderLine, where does it come from?
    productCode = (toProductCode checkProductCodeExists . UnvalidatedOrderLine.productCode) unvalidatedOrderLine
    quantity = toOrderQuantity productCode (UnvalidatedOrderLine.quantity unvalidatedOrderLine)
  in
    left (ValidationError "TODO field name") $ OrderLine.OrderLine <$> orderLineId <*> orderId <*> pure productCode <*> pure quantity -- TODO review pure + ap usage here

createBillingEvent :: PO.PricedOrder -> Maybe BillableOrderPlaced.BillableOrderPlaced
createBillingEvent placedOrder =
  let
    billingAmount = (BillingAmount.value . PO.amountToBill) placedOrder
  in
    if billingAmount > 0 then
      Just $ BillableOrderPlaced.BillableOrderPlaced
        (PO.orderId placedOrder)
        (PO.billingAddress placedOrder)
        (PO.amountToBill placedOrder)
    else Nothing

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

-- (port) Force unwrap Maybe. Aeson parser returns Maybe while original repo's returns actual type.
-- So we force unwrap it, to keep differences with repo minimal.
unsafeUnwrap :: Maybe a -> a
unsafeUnwrap (Just a) = a
unsafeUnwrap Nothing = error "Maybe is Nothing"

sequence :: [Either a b] -> Either a [b]
sequence =
  foldr prepend (Right [])

prepend :: Either a b -> Either a [b] -> Either a [b]
prepend (Right first) (Right rest) = Right (first : rest)
prepend (Left err1) (Right _) = Left err1
prepend (Right _) (Left err2) = Left err2
prepend (Left err1) (Left _) = Left err1
