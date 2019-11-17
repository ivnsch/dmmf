
module PlaceOrderWorkflow where

import Prelude hiding (lines, map)
import Data.List.NonEmpty as NEL
import SharedTypes
import qualified ValidatedOrder
import qualified UnvalidatedOrder
import qualified PricedOrder
import qualified OrderLine
import qualified OrderId
import qualified BillingAmount
import qualified PricedOrder as PO
import qualified OrderAcknowledgment
import qualified OrderAcknowledgmentSent
import qualified CustomerInfo
import qualified Address
import qualified OrderLineId
import qualified UnvalidatedOrderLine
import qualified CheckedAddress
import qualified UnvalidatedCustomerInfo
import qualified PersonalName
import String50
import qualified ProductCode
import OrderQuantity
import qualified UnitQuantity
import qualified KilogramQuantity
import qualified PricedOrderLine as POL
import Price

type OrderPlaced = PricedOrder.PricedOrder 
type PlaceOrder = Command UnvalidatedOrder.UnvalidatedOrder

data BillableOrderPlaced = BillableOrderPlaced {
  orderId :: OrderId.OrderId,
  billingAddress :: Address.Address,
  amountToBill :: BillingAmount.BillingAmount
}

data Order = Unvalidated UnvalidatedOrder.UnvalidatedOrder | Validated ValidatedOrder.ValidatedOrder | Priced PricedOrder.PricedOrder 

data PlaceOrderEvents = PlaceOrderEvents {
  acknowledgmentSent :: OrderAcknowledgment.OrderAcknowledgment,
  orderPlaced :: OrderPlaced,
  billableOrderPlaced :: BillableOrderPlaced
}

data PlaceOrderError = ValidationErrors [ValidationError] | NotDefinedYet

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

type CheckAddressExists = UnvalidatedAddress -> CheckedAddress.CheckedAddress
checkAddressExists :: CheckAddressExists
checkAddressExists unvalidatedAddress = undefined

toCustomerInfo :: UnvalidatedCustomerInfo.UnvalidatedCustomerInfo -> CustomerInfo.CustomerInfo
toCustomerInfo unvalidatedCustomerInfo = 
  let 
    firstName = string50 $ UnvalidatedCustomerInfo.firstName unvalidatedCustomerInfo
    lastName = string50 $ UnvalidatedCustomerInfo.lastName unvalidatedCustomerInfo
    emailAddress = EmailAddress $ UnvalidatedCustomerInfo.emailAddress unvalidatedCustomerInfo
    name = PersonalName.PersonalName firstName lastName
  in
    CustomerInfo.CustomerInfo name emailAddress

toAddress :: CheckAddressExists -> UnvalidatedAddress -> Address.Address
toAddress checkAddressExists unvalidatedAddress =
  let
    checkedAddress = checkAddressExists unvalidatedAddress
    addressLine1 = string50 $ CheckedAddress.addressLine1 checkedAddress
    addressLine2 = string50 $ CheckedAddress.addressLine2 checkedAddress
    addressLine3 = string50 $ CheckedAddress.addressLine3 checkedAddress
    addressLine4 = string50 $ CheckedAddress.addressLine4 checkedAddress
    city = City $ string50 $ CheckedAddress.city checkedAddress
    zipCode = ZipCode $ CheckedAddress.zipCode checkedAddress
  in
    Address.Address addressLine1 addressLine2 addressLine3 addressLine4 city zipCode

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

validateOrder :: CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder.UnvalidatedOrder -> ValidatedOrder.ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder = 
  let
    orderId = OrderId.create $ UnvalidatedOrder.orderId unvalidatedOrder 
    customerInfo = toCustomerInfo $ UnvalidatedOrder.customerInfo unvalidatedOrder
    shippingAddress = toAddress checkAddressExists $ UnvalidatedOrder.shippingAddress unvalidatedOrder
    billingAddress = toAddress checkAddressExists $ UnvalidatedOrder.shippingAddress unvalidatedOrder
    orderLines = map (toValidatedOrderLine checkProductCodeExists) $ UnvalidatedOrder.orderLines unvalidatedOrder
    validatedOrder = ValidatedOrder.ValidatedOrder orderId customerInfo shippingAddress billingAddress orderLines
  in
    validatedOrder

type ToValidatedOrderLine = CheckProductCodeExists -> UnvalidatedOrderLine.UnvalidatedOrderLine -> OrderLine.OrderLine
toValidatedOrderLine checkProductCodeExists unvalidatedOrderLine =
  let
    orderLineId = OrderLineId.create $ UnvalidatedOrderLine.orderLineId unvalidatedOrderLine
    orderId = OrderId.create $ UnvalidatedOrderLine.orderId unvalidatedOrderLine
    productCode = toProductCode checkProductCodeExists $ UnvalidatedOrderLine.productCode unvalidatedOrderLine
    quantity = toOrderQuantity productCode (UnvalidatedOrderLine.quantity unvalidatedOrderLine)
  in
    OrderLine.OrderLine orderLineId orderId productCode quantity

toPricedOrderLine :: GetProductPrice -> OrderLine.OrderLine -> POL.PricedOrderLine
toPricedOrderLine getProductPrice line =
  let
    qty = OrderQuantity.value $ OrderLine.quantity line
    price = getProductPrice $ OrderLine.productCode line
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

priceOrder :: GetProductPrice -> ValidatedOrder.ValidatedOrder -> PO.PricedOrder
priceOrder getProductPrice validatedOrder = 
  let
    lines = map (toPricedOrderLine getProductPrice) $ ValidatedOrder.orderLines validatedOrder
    sumPrice = sumPrices $ map POL.price lines
    amountToBill = BillingAmount.BillingAmount $ case sumPrice of Price value -> value
  in
    PO.PricedOrder
      (ValidatedOrder.orderId validatedOrder) 
      (ValidatedOrder.customerInfo validatedOrder)
      (ValidatedOrder.shippingAddress validatedOrder)
      (ValidatedOrder.billingAddress validatedOrder) 
      lines 
      amountToBill

placeOrder :: UnvalidatedOrder.UnvalidatedOrder -> [PlaceOrderEvent]
placeOrder unvalidatedOrder =
  let
    validateOrder' = validateOrder checkProductCodeExists checkAddressExists
    priceOrder' = priceOrder getProductPrice
    acknowledgeOrder' = acknowledgeOrder createAcknowledgmentLetter sendAcknowledgment

    validatedOrder = validateOrder' unvalidatedOrder
    pricedOrder = priceOrder' validatedOrder
    acknowledmentOption = acknowledgeOrder' pricedOrder
  in
    createEvents pricedOrder acknowledmentOption

createBillingEvent :: PO.PricedOrder -> Maybe BillableOrderPlaced
createBillingEvent placedOrder = 
  let
    billingAmount = BillingAmount.value $ PO.amountToBill placedOrder
  in
    if billingAmount > 0 then
      Just $ BillableOrderPlaced 
        (PO.orderId placedOrder) 
        (PO.billingAddress placedOrder)
        (PO.amountToBill placedOrder)
    else Nothing

