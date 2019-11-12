{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- TODO rename in OrderTaking.Domain (file too?)
module OrderTakingDomain where

import OrderQuantity
-- import qualified Data.List as L (find)
import qualified Data.List as L
import Prelude hiding (lines, map)
import Data.List.NonEmpty as NEL
import Data.Time
import qualified PricedOrderLine as POL
import qualified PricedOrder as PO
import SharedTypes

-- data Foo = Foo {
--   myField :: Int
-- }
-- data Bar = Bar {
--   myField :: Int
-- }
-- myFunc :: Foo -> Int
-- myFunc foo = (myField :: Foo -> Int) foo -- Ambiguous occurrence "myField"


data OrderLine = OrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productId :: ProductId,
  orderQuantity :: Int
} deriving (Show)

-- data Foo = Foo {
--   lines :: [String],
--   bla :: String
-- }
-- myfunc :: Foo -> [String]
-- myfunc foo = (L.lines :: Foo -> [String]) foo

orderKey :: OrderLine -> (OrderId, ProductId)
orderKey orderLine = ((orderId :: OrderLine -> OrderId) orderLine, productId orderLine) :: (OrderId, ProductId)

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

data Order = Unvalidated UnvalidatedOrder | Validated ValidatedOrder | Priced PO.PricedOrder 

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: UnvalidatedAddress
}

data ValidatedOrder = ValidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: ShippingAddress,
  billingAddress :: BillingAddress,
  orderLines :: NonEmpty OrderLine
}

data PlaceOrderEvents = PlaceOrderEvents {
  acknowledgmentSent :: AcknowledgmentSent,
  orderPlaced :: OrderPlaced,
  billableOrderPlaced :: BillableOrderPlaced
}

data PlaceOrderError = ValidationErrors [ValidationError] | NotDefinedYet

data ValidationError = ValidationError {
  fieldName :: String,
  errorDescription :: String
}

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

data Command a = Command {
  commandData :: a,
  timeStamp :: UTCTime,
  userId :: CustomerId
}

type PlaceOrder = Command UnvalidatedOrder

-- TODO
-- data OrderTakingCommand = Place PlaceOrder | Change ChangeOrder | Cancel CancelOrder


-- instance Eq Contact where
--   x == y = contactId x == contactId y
-- TODO Hashable?

findOrderLine :: NonEmpty POL.PricedOrderLine -> OrderLineId -> Maybe POL.PricedOrderLine
findOrderLine orderLines olId =
  L.find (\ol -> POL.orderLineId ol == olId) orderLines 

replaceOrderLine :: NonEmpty POL.PricedOrderLine -> OrderLineId -> POL.PricedOrderLine -> NonEmpty POL.PricedOrderLine
-- TODO optimize
replaceOrderLine orderLines oldId newOrderLine = 
  map (\ol -> if POL.orderLineId ol == oldId then newOrderLine else ol) orderLines

toBillingAmount :: Price -> BillingAmount
toBillingAmount (Price value) = BillingAmount value

toPriceValue :: Price -> Double
toPriceValue (Price value) = value

calculateTotalPrice :: NonEmpty POL.PricedOrderLine -> Price
calculateTotalPrice orderLines = 
  Price $ foldr (\l a -> toPriceValue (POL.price l) + a) 0 orderLines

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

