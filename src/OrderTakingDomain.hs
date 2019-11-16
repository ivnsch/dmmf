{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- TODO rename in OrderTaking.Domain (file too?)
module OrderTakingDomain where

import OrderQuantity
-- import qualified Data.List as L (find)
import qualified Data.List as L
import Prelude hiding (lines, map)
import Data.List.NonEmpty as NEL
import qualified PricedOrderLine as POL
import qualified PricedOrder as PO
import qualified OrderAcknowledgment
import qualified OrderAcknowledgmentSent
import SharedTypes
import PlaceOrderWorkflow
import qualified UnvalidatedOrder
import qualified ValidatedOrder

-- data Foo = Foo {
--   myField :: Int
-- }
-- data Bar = Bar {
--   myField :: Int
-- }
-- myFunc :: Foo -> Int
-- myFunc foo = (myField :: Foo -> Int) foo -- Ambiguous occurrence "myField"

-- data Foo = Foo {
--   lines :: [String],
--   bla :: String
-- }
-- myfunc :: Foo -> [String]
-- myfunc foo = (L.lines :: Foo -> [String]) foo


data CreditCardInfo = CreditCardInfo { 
  cardType :: CardType,
  cardNumber :: CardType
} deriving (Show)


-- data Order = Order {
--   orderId :: OrderId,
--   customerId :: CustomerId,
--   shippingAddress :: ShippingAddress,
--   billingAddress :: BillingAddress,
--   lines :: NonEmpty OrderLine,
--   amountToBill :: BillingAmount
-- }


data Invoice = UnpaidInvoice {
  invoiceId :: InvoiceId
} | PaidInvoice {
  invoiceId :: InvoiceId
} deriving (Show)

data BothContactMethods = BothContactMethods {
  email :: EmailContactInfo,
  address :: PostalContactInfo
}
data ContactInfo = EmailOnly EmailContactInfo | AddrOnly PostalContactInfo | EmailAndAddr BothContactMethods

data Contact = Contact {
  contactId :: ContactId,
  name :: String,
  contactInfo :: ContactInfo
}

-- data PlaceOrder = PlaceOrder {
--   orderForm :: UnvalidatedOrder,
--   timeStamp :: UTCTime,
--   userId :: CustomerId
-- }


-- TODO
-- data OrderTakingCommand = Place PlaceOrder | Change ChangeOrder | Cancel CancelOrder


-- instance Eq Contact where
--   x == y = contactId x == contactId y
-- TODO Hashable?

toBillingAmount :: Price -> BillingAmount
toBillingAmount (Price value) = BillingAmount value

toPriceValue :: Price -> Double
toPriceValue (Price value) = value

calculateTotalPrice :: NonEmpty POL.PricedOrderLine -> Price
calculateTotalPrice orderLines = 
  Price $ foldr (\l a -> toPriceValue (POL.price l) + a) 0 orderLines

findOrderLine :: NonEmpty POL.PricedOrderLine -> OrderLineId -> Maybe POL.PricedOrderLine
findOrderLine orderLines olId =
  L.find (\ol -> POL.orderLineId ol == olId) orderLines 

replaceOrderLine :: NonEmpty POL.PricedOrderLine -> OrderLineId -> POL.PricedOrderLine -> NonEmpty POL.PricedOrderLine
-- TODO optimize
replaceOrderLine orderLines oldId newOrderLine = 
  map (\ol -> if POL.orderLineId ol == oldId then newOrderLine else ol) orderLines

changeOrderPrice :: PO.PricedOrder -> OrderLineId -> Price -> Maybe PO.PricedOrder
changeOrderPrice order orderLineId newPrice =
  let 
    newOrderLines = updateOrderLinesPrice order orderLineId newPrice
  in 
    (\nols -> order { PO.orderLines = nols }) <$> newOrderLines
    
-- This was added after adding amountToBill to Order. changeOrderPrice is "deprecated" now.
changeOrderLinePrice :: PO.PricedOrder -> OrderLineId -> Price -> Maybe PO.PricedOrder
changeOrderLinePrice order orderLineId newPrice =
  let 
    newOrderLines = updateOrderLinesPrice order orderLineId newPrice
    newTotalPrice = calculateTotalPrice <$> newOrderLines
    newAmountToBill = BillingAmount . toPriceValue <$> newTotalPrice
  in 
    (\nols natb -> order { PO.orderLines = nols, PO.amountToBill = natb }) <$> newOrderLines <*> newAmountToBill

-- Helper function to fix HLint repeated code warning 
updateOrderLinesPrice :: PO.PricedOrder -> OrderLineId -> Price -> Maybe (NonEmpty POL.PricedOrderLine)
updateOrderLinesPrice order orderLineId newPrice = 
  let 
    orderLine = findOrderLine (PO.orderLines order) orderLineId
    newOrderLine = (\ol -> ol { POL.price = newPrice }) <$> orderLine
  in
    replaceOrderLine (PO.orderLines order) orderLineId <$> newOrderLine 

checkProductCodeExists :: ProductCode -> Bool
checkProductCodeExists productCode = undefined

checkAddressExists :: UnvalidatedAddress -> Bool
checkAddressExists unvalidatedAddress = undefined

getProductPrice :: ProductCode -> Price
getProductPrice product = undefined

type CheckProductCodeExists = ProductCode -> Bool
type CheckAddressExists = UnvalidatedAddress -> Bool
validateOrder :: CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder.UnvalidatedOrder 
  -> Either String ValidatedOrder.ValidatedOrder
validateOrder = undefined

type GetProductPrice = ProductCode -> Price
priceOrder :: GetProductPrice -> ValidatedOrder.ValidatedOrder -> Either PricingError PricedOrder
priceOrder = undefined

-- createOrderAcknowledgmentLetter :: PricedOrder -> HTMLString
-- createOrderAcknowledgmentLetter = undefined

type CreateOrderAcknowledgmentLetter = PricedOrder -> HTMLString

type SendOrderAcknowledgment = OrderAcknowledgment.OrderAcknowledgment -> SendResult

acknowledgeOrder :: CreateOrderAcknowledgmentLetter -> SendOrderAcknowledgment -> PricedOrder -> 
  Maybe OrderAcknowledgmentSent.OrderAcknowledgmentSent
acknowledgeOrder = undefined
 
type CreateEvents = PricedOrder -> [PlaceOrderEvents]

printQuantity qt =
  case qt of
    UnitQuantity q -> print q
    KilogramQuantity q -> print q

printList :: Show a => [a] -> String
printList list =
  case list of
    [] -> "empty!"
    [x] -> "one!" <> show x
    [x,y] -> "two!" <> show x <> ", " <> show y
    (x:xs) -> "other?? head: " <> show x <> "rest: " <> show xs

