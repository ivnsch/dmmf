{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- TODO rename in OrderTaking.Domain (file too?)
module OrderTakingDomain where

import OrderQuantity
import qualified Data.List as L (find)
import Prelude hiding (lines)
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
newtype PhoneNumber = PhoneNumber String deriving (Show)
newtype BillingAmount = BillingAmount Double deriving (Show)
newtype Price = Price Double deriving (Show)

data ProductCode = WidgetCode String | GizmoCode String deriving (Show)
data CardType = Visa | Master deriving (Show)

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

data Order = Order {
  orderId :: OrderId,
  customerId :: CustomerId,
  shippingAddress :: ShippingAddress,
  billingAddress :: BillingAddress,
  lines :: [OrderLine],
  amountToBill :: BillingAmount
}

data UnvalidatedOrder = UnvalidatedOrder {
  orderId :: String,
  customerInfo :: String,
  shippingAddress :: String
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

data Contact = Contact {
  contactId :: ContactId,
  phoneNumber :: PhoneNumber,
  emailAddress :: EmailAddress
}

-- instance Eq Contact where
--   x == y = contactId x == contactId y
-- TODO Hashable?

findOrderLine :: [OrderLine] -> OrderLineId -> Maybe OrderLine
findOrderLine orderLines olId =
  L.find (\ol -> (orderLineId ol) == olId) orderLines 

replaceOrderLine :: [OrderLine] -> OrderLineId -> OrderLine -> [OrderLine]
-- TODO optimize
replaceOrderLine orderLines oldId newOrderLine = 
  map (\ol -> if ((orderLineId ol) == oldId) then newOrderLine else ol) orderLines

changeOrderPrice :: Order -> OrderLineId -> Price -> Maybe Order
changeOrderPrice order orderLineId newPrice =
  let 
    orderLine = findOrderLine (lines order) orderLineId
    newOrderLine = (\ol -> ol { price = newPrice }) <$> orderLine
    newOrderLines = (\nol -> replaceOrderLine (lines order) orderLineId nol) <$> newOrderLine
  in 
    (\nols -> order { lines = nols }) <$> newOrderLines

printQuantity qt =
  case qt of
    UnitQuantity q -> putStrLn $ show q
    KilogramQuantity q -> putStrLn $ show q

printList :: Show a => [a] -> String
printList list =
  case list of
    [] -> "empty!"
    [x] -> "one!" <> show x
    [x,y] -> "two!" <> show x <> ", " <> show y
    (x:xs) -> "other?? head: " <> show x <> "rest: " <> show xs

