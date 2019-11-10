{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- TODO rename in OrderTaking.Domain (file too?)
module OrderTakingDomain where

import OrderQuantity
import qualified Data.List as L (find)
import Prelude hiding (lines, map)
import Data.List.NonEmpty as NEL

-- data Foo = Foo {
--   myField :: Int
-- }
-- data Bar = Bar {
--   myField :: Int
-- }
-- myFunc :: Foo -> Int
-- myFunc foo = (myField :: Foo -> Int) foo -- Ambiguous occurrence "myField"

newtype OrderId = OrderId String deriving (Eq, Show)
newtype OrderLineId = OrderLineId String deriving (Eq, Show)
newtype ProductId = ProductId String deriving (Eq, Show)

data OrderLine = OrderLine {
  orderLineId :: OrderLineId,
  orderId :: OrderId,
  productId :: ProductId,
  orderQuantity :: Int,
  price :: Price
} deriving (Show)


-- data Foo = Foo {
--   lines :: [String]
-- }
-- myfunc :: Foo -> [String]
-- myfunc foo = ((lines :: Foo -> [String]) foo)

orderKey :: OrderLine -> (OrderId, ProductId)
orderKey orderLine = ((orderId :: OrderLine -> OrderId) orderLine, productId orderLine) :: (OrderId, ProductId)

newtype CustomerId = CustomerId Int deriving (Show)
newtype InvoiceId = InvoiceId Int deriving (Show)
newtype ContactId = ContactId Int deriving (Show, Eq)
newtype EmailAddress = EmailAddress String deriving (Show)
newtype VerifiedEmailAddress = VerifiedEmailAddress String deriving (Show)
newtype PhoneNumber = PhoneNumber String deriving (Show)
newtype BillingAmount = BillingAmount Double deriving (Show)
newtype Price = Price Double deriving (Show)
newtype UnvalidatedAddress = UnvalidatedAddress String deriving Show
newtype ValidatedAddress = ValidatedAddress String deriving Show

data ProductCode = WidgetCode String | GizmoCode String deriving (Show)
data CardType = Visa | Master deriving (Show)

data CustomerEmail = Unverfied EmailAddress | Verified VerifiedEmailAddress deriving Show

data CreditCardInfo = CreditCardInfo { 
  cardType :: CardType,
  cardNumber :: CardType
} deriving (Show)

data ShippingAddress = ShippingAddress {
}

data BillingAddress = BillingAddress {
}

-- TODO
newtype AcknowledgmentSent = AcknowledgmentSent String
newtype OrderPlaced = OrderPlaced String
newtype BillableOrderPlaced = BillableOrderPlaced String
newtype EmailContactInfo = EmailContactInfo String deriving Show
newtype PostalContactInfo = PostalContactInfo String deriving Show

data Order = Order {
  orderId :: OrderId,
  customerId :: CustomerId,
  shippingAddress :: ShippingAddress,
  billingAddress :: BillingAddress,
  lines :: NonEmpty OrderLine,
  amountToBill :: BillingAmount
}

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: UnvalidatedAddress
}

data ValidatedOrder = ValidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: ValidatedAddress
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

-- instance Eq Contact where
--   x == y = contactId x == contactId y
-- TODO Hashable?

findOrderLine :: NonEmpty OrderLine -> OrderLineId -> Maybe OrderLine
findOrderLine orderLines olId =
  L.find (\ol -> orderLineId ol == olId) orderLines 

replaceOrderLine :: NonEmpty OrderLine -> OrderLineId -> OrderLine -> NonEmpty OrderLine
-- TODO optimize
replaceOrderLine orderLines oldId newOrderLine = 
  map (\ol -> if orderLineId ol == oldId then newOrderLine else ol) orderLines

toBillingAmount :: Price -> BillingAmount
toBillingAmount (Price value) = BillingAmount value

toPriceValue :: Price -> Double
toPriceValue (Price value) = value

calculateTotalPrice :: NonEmpty OrderLine -> Price
calculateTotalPrice orderLines = 
  Price $ foldr (\l a -> toPriceValue (price l) + a) 0 orderLines

changeOrderPrice :: Order -> OrderLineId -> Price -> Maybe Order
changeOrderPrice order orderLineId newPrice =
  let 
    newOrderLines = updateOrderLinesPrice order orderLineId newPrice
  in 
    (\nols -> order { lines = nols }) <$> newOrderLines
    
-- This was added after adding amountToBill to Order. changeOrderPrice is "deprecated" now.
changeOrderLinePrice :: Order -> OrderLineId -> Price -> Maybe Order
changeOrderLinePrice order orderLineId newPrice =
  let 
    newOrderLines = updateOrderLinesPrice order orderLineId newPrice
    newTotalPrice = calculateTotalPrice <$> newOrderLines
    newAmountToBill = BillingAmount . toPriceValue <$> newTotalPrice
  in 
    (\nols natb -> order { lines = nols, amountToBill = natb }) <$> newOrderLines <*> newAmountToBill

-- Helper function to fix HLint repeated code warning 
updateOrderLinesPrice :: Order -> OrderLineId -> Price -> Maybe (NonEmpty OrderLine)
updateOrderLinesPrice order orderLineId newPrice = 
  let 
    orderLine = findOrderLine (lines order) orderLineId
    newOrderLine = (\ol -> ol { price = newPrice }) <$> orderLine
  in
    replaceOrderLine (lines order) orderLineId <$> newOrderLine 

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

