
module PlaceOrderWorkflow where

import Prelude hiding (lines, map, sequence)
import Data.List.NonEmpty as NE
import qualified NonEmptyExt as NEE
import SharedTypes
import qualified Types.ValidatedOrder as ValidatedOrder
import qualified Types.UnvalidatedOrder as UnvalidatedOrder
import qualified Types.OrderLine as OrderLine
import qualified Types.OrderId as OrderId
import qualified Types.BillingAmount as BillingAmount
import qualified Types.PricedOrder as PO
import qualified Types.OrderAcknowledgment as OrderAcknowledgment
import qualified Types.OrderAcknowledgmentSent as OrderAcknowledgmentSent
import qualified Types.CustomerInfo as CustomerInfo
import qualified Types.Address as Address
import qualified Types.OrderLineId as OrderLineId
import qualified Types.UnvalidatedOrderLine as UnvalidatedOrderLine
import qualified Types.CheckedAddress as CheckedAddress
import qualified Types.UnvalidatedCustomerInfo as UnvalidatedCustomerInfo
import qualified Types.PersonalName as PersonalName
import Types.String50
import qualified Types.ProductCode as ProductCode
import Types.OrderQuantity as OrderQuantity
import qualified Types.UnitQuantity as UnitQuantity
import qualified Types.KilogramQuantity as KilogramQuantity
import qualified Types.PricedOrderLine as POL
import Types.Price as Price
import Control.Arrow(left)

type OrderPlaced = PO.PricedOrder 
type PlaceOrder = Command UnvalidatedOrder.UnvalidatedOrder

data BillableOrderPlaced = BillableOrderPlaced {
  orderId :: OrderId.OrderId,
  billingAddress :: Address.Address,
  amountToBill :: BillingAmount.BillingAmount
}

data Order = Unvalidated UnvalidatedOrder.UnvalidatedOrder | Validated ValidatedOrder.ValidatedOrder | Priced PO.PricedOrder 

data PlaceOrderEvents = PlaceOrderEvents {
  acknowledgmentSent :: OrderAcknowledgment.OrderAcknowledgment,
  orderPlaced :: OrderPlaced,
  billableOrderPlaced :: BillableOrderPlaced
}


-- data PlaceOrderResult = PlaceOrderResult {
--   orderPlaced :: OrderPlaced,
--   billableOrderPlaced :: BillableOrderPlaced,
--   orderAcknowledgmentSent :: OrderAcknowledgmentSent.OrderAcknowledgmentSent 
-- }

data PlaceOrderEvent = OrderPlacedEvent OrderPlaced | BillableOrderPlacedEvent BillableOrderPlaced | AcknowledgmentSentEvent OrderAcknowledgmentSent.OrderAcknowledgmentSent

type CreateAcknowledgmentLetter = PO.PricedOrder -> HTMLString
createAcknowledgmentLetter :: CreateAcknowledgmentLetter
createAcknowledgmentLetter = undefined

type SendOrderAcknowledgment = OrderAcknowledgment.OrderAcknowledgment -> SendResult
sendAcknowledgment :: SendOrderAcknowledgment
sendAcknowledgment acknowledgment = undefined

acknowledgeOrder :: CreateAcknowledgmentLetter -> SendOrderAcknowledgment -> PO.PricedOrder -> 
  Maybe OrderAcknowledgmentSent.OrderAcknowledgmentSent
acknowledgeOrder createAcknowledgmentLetter sendAcknowledgment pricedOrder = 
  let
    letter = createAcknowledgmentLetter pricedOrder
    acknowledgment = OrderAcknowledgment.OrderAcknowledgment (CustomerInfo.emailAddress $ PO.customerInfo pricedOrder) letter
  in
    case sendAcknowledgment acknowledgment of
      Sent -> Just $ OrderAcknowledgmentSent.OrderAcknowledgmentSent (PO.orderId pricedOrder) (CustomerInfo.emailAddress $ PO.customerInfo pricedOrder)
      NotSent -> Nothing
 
-- type CreateEvents = PO.PricedOrder -> [PlaceOrderEvent]
createEvents :: PO.PricedOrder -> Maybe OrderAcknowledgmentSent.OrderAcknowledgmentSent -> [PlaceOrderEvent]
createEvents pricedOrder acknowledgmentEventOpt =
  let
    events1 = [OrderPlacedEvent pricedOrder]
    events2 = listOfOpt $ AcknowledgmentSentEvent <$> acknowledgmentEventOpt
    events3 = listOfOpt $ BillableOrderPlacedEvent <$> createBillingEvent pricedOrder
  in
    events1 <> events2 <> events3

listOfOpt :: Maybe a -> [a]
listOfOpt (Just value) = [value]
listOfOpt Nothing = []

type CheckAddressExists = UnvalidatedAddress -> Either ValidationError CheckedAddress.CheckedAddress
checkAddressExists :: CheckAddressExists
checkAddressExists unvalidatedAddress = undefined

toCustomerInfo :: UnvalidatedCustomerInfo.UnvalidatedCustomerInfo -> Either ValidationError CustomerInfo.CustomerInfo
toCustomerInfo unvalidatedCustomerInfo = 
  let 
    firstName = (string50 . UnvalidatedCustomerInfo.firstName) unvalidatedCustomerInfo
    lastName = (string50 . UnvalidatedCustomerInfo.lastName) unvalidatedCustomerInfo
    emailAddress = (EmailAddress . UnvalidatedCustomerInfo.emailAddress) unvalidatedCustomerInfo
    name = PersonalName.PersonalName firstName lastName
  in
    Right $ CustomerInfo.CustomerInfo name emailAddress

toAddress :: CheckAddressExists -> UnvalidatedAddress -> Either ValidationError Address.Address
toAddress checkAddressExists unvalidatedAddress =
  let
    checkedAddress = checkAddressExists unvalidatedAddress
    addressLine1 = string50 . CheckedAddress.addressLine1 <$> checkedAddress
    addressLine2 = string50 . CheckedAddress.addressLine2 <$> checkedAddress
    addressLine3 = string50 . CheckedAddress.addressLine3 <$> checkedAddress
    addressLine4 = string50 . CheckedAddress.addressLine4 <$> checkedAddress
    city = City . string50 . CheckedAddress.city <$> checkedAddress
    zipCode = ZipCode . CheckedAddress.zipCode <$> checkedAddress
  in
    Address.Address <$> addressLine1 <*> addressLine2 <*> addressLine3 <*> addressLine4 <*> city <*> zipCode

toProductCode :: CheckProductCodeExists -> String -> ProductCode.ProductCode
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

-- type CheckProductCodeExists = ProductCode.ProductCode -> Bool
type CheckProductCodeExists = String -> Bool
checkProductCodeExists :: CheckProductCodeExists
checkProductCodeExists productCode = undefined

validateOrder :: CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder.UnvalidatedOrder -> Either ValidationError ValidatedOrder.ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder = 
  let
    orderId = (OrderId.create . UnvalidatedOrder.orderId) unvalidatedOrder 
    orderIdWithLeftValidationError = left (ValidationError "OrderId") orderId -- TODO compare with repo
    customerInfo = (toCustomerInfo . UnvalidatedOrder.customerInfo) unvalidatedOrder
    shippingAddress = (toAddress checkAddressExists . UnvalidatedOrder.shippingAddress) unvalidatedOrder
    billingAddress = (toAddress checkAddressExists . UnvalidatedOrder.shippingAddress) unvalidatedOrder
    orderLines = (map (toValidatedOrderLine checkProductCodeExists) . UnvalidatedOrder.orderLines) unvalidatedOrder
  in
    ValidatedOrder.ValidatedOrder <$> orderIdWithLeftValidationError <*> customerInfo <*> shippingAddress <*> billingAddress <*> NEE.sequence orderLines

validateOrderAdapted :: CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder.UnvalidatedOrder -> Either PlaceOrderError ValidatedOrder.ValidatedOrder
validateOrderAdapted checkProductCodeExists checkAddressExists unvalidatedOrder = 
  left Validation $ validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder

toValidatedOrderLine :: CheckProductCodeExists -> UnvalidatedOrderLine.UnvalidatedOrderLine -> Either ValidationError OrderLine.OrderLine
toValidatedOrderLine checkProductCodeExists unvalidatedOrderLine =
  let
    orderLineId = (OrderLineId.create . UnvalidatedOrderLine.orderLineId) unvalidatedOrderLine
    orderId = (OrderId.create . UnvalidatedOrderLine.orderId) unvalidatedOrderLine
    productCode = (toProductCode checkProductCodeExists . UnvalidatedOrderLine.productCode) unvalidatedOrderLine
    quantity = toOrderQuantity productCode (UnvalidatedOrderLine.quantity unvalidatedOrderLine)
  in
    left (ValidationError "TODO field name") $ OrderLine.OrderLine <$> orderLineId <*> orderId <*> pure productCode <*> pure quantity -- TODO review pure + ap usage here

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

sumPrices :: NonEmpty Price -> Price
sumPrices prices = Price $ foldr (\p a -> case p of Price value  -> value + a) 0 prices

type GetProductPrice = ProductCode.ProductCode -> Price
getProductPrice :: GetProductPrice
getProductPrice product = undefined

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

-- TODO computation expressions -> do notation? Go through usages of >>= and see if it's worth replacing with do

placeOrder :: UnvalidatedOrder.UnvalidatedOrder -> Either PlaceOrderError [PlaceOrderEvent]
placeOrder unvalidatedOrder =
  let
    validateOrder' = validateOrderAdapted checkProductCodeExists checkAddressExists
    priceOrder' = priceOrderAdapted getProductPrice
    acknowledgeOrder' = acknowledgeOrder createAcknowledgmentLetter sendAcknowledgment

    validatedOrder = validateOrder' unvalidatedOrder
    pricedOrder = validatedOrder >>= priceOrder'
    acknowledmentOption = acknowledgeOrder' <$> pricedOrder
  in
    createEvents <$> pricedOrder <*> acknowledmentOption

createBillingEvent :: PO.PricedOrder -> Maybe BillableOrderPlaced
createBillingEvent placedOrder = 
  let
    billingAmount = (BillingAmount.value . PO.amountToBill) placedOrder
  in
    if billingAmount > 0 then
      Just $ BillableOrderPlaced 
        (PO.orderId placedOrder) 
        (PO.billingAddress placedOrder)
        (PO.amountToBill placedOrder)
    else Nothing
