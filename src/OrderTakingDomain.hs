-- TODO rename in OrderTaking.Domain (file too?)
module OrderTakingDomain where

import Types.OrderQuantity as OrderQuantity
-- import qualified Data.List as L (find)
import qualified Data.List as L
import Prelude hiding (lines)
--import Data.List.NonEmpty as NEL
import qualified Types.PricedOrderLine as POL
import qualified Types.PricedOrder as PO
import qualified Types.OrderAcknowledgment as OrderAcknowledgment
import qualified Types.OrderAcknowledgmentSent as OrderAcknowledgmentSent
import SharedTypes
import qualified Types.UnvalidatedOrder as UnvalidatedOrder
import qualified Types.ValidatedOrder as ValidatedOrder
import qualified Types.OrderId as OrderId
import Types.String50
import qualified Types.UnvalidatedCustomerInfo
import qualified Types.CustomerInfo as CustomerInfo
import qualified Types.PersonalName as PersonalName
import qualified Types.CheckedAddress as CheckedAddress
import qualified Types.Address as Address
import qualified Types.UnvalidatedOrderLine
import qualified Types.OrderLineId as OrderLineId
import qualified Types.OrderLine as OrderLine
import qualified Types.UnitQuantity as UnitQuantity
import qualified Types.KilogramQuantity as KilogramQuantity
import qualified Types.ProductCode as ProductCode
import qualified Types.PricedOrderLine
import Types.Price as Price
import qualified Types.BillingAmount as BillingAmount

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

toBillingAmount :: Price -> BillingAmount.BillingAmount
toBillingAmount (Price value) = BillingAmount.BillingAmount value

toPriceValue :: Price -> Double
toPriceValue (Price value) = value

calculateTotalPrice :: [POL.PricedOrderLine] -> Price
calculateTotalPrice orderLines = 
  Price $ foldr (\l a -> toPriceValue (POL.linePrice l) + a) 0 orderLines

findOrderLine :: [POL.PricedOrderLine] -> OrderLineId.OrderLineId -> Maybe POL.PricedOrderLine
findOrderLine orderLines olId =
  L.find (\ol -> POL.orderLineId ol == olId) orderLines 

replaceOrderLine :: [POL.PricedOrderLine] -> OrderLineId.OrderLineId -> POL.PricedOrderLine -> [POL.PricedOrderLine]
-- TODO optimize
replaceOrderLine orderLines oldId newOrderLine = 
  map (\ol -> if POL.orderLineId ol == oldId then newOrderLine else ol) orderLines

changeOrderPrice :: PO.PricedOrder -> OrderLineId.OrderLineId -> Price -> Maybe PO.PricedOrder
changeOrderPrice order orderLineId newPrice =
  let 
    newOrderLines = updateOrderLinesPrice order orderLineId newPrice
  in 
    (\nols -> order { PO.lines = nols }) <$> newOrderLines
    
-- This was added after adding amountToBill to Order. changeOrderPrice is "deprecated" now.
changeOrderLinePrice :: PO.PricedOrder -> OrderLineId.OrderLineId -> Price -> Maybe PO.PricedOrder
changeOrderLinePrice order orderLineId newPrice =
  let 
    newOrderLines = updateOrderLinesPrice order orderLineId newPrice
    newTotalPrice = calculateTotalPrice <$> newOrderLines
    newAmountToBill = BillingAmount.BillingAmount . toPriceValue <$> newTotalPrice
  in 
    (\nols natb -> order { PO.lines = nols, PO.amountToBill = natb }) <$> newOrderLines <*> newAmountToBill

-- Helper function to fix HLint repeated code warning 
updateOrderLinesPrice :: PO.PricedOrder -> OrderLineId.OrderLineId -> Price -> Maybe [POL.PricedOrderLine]
updateOrderLinesPrice order orderLineId newPrice = 
  let 
    orderLine = findOrderLine (PO.lines order) orderLineId
    newOrderLine = (\ol -> ol { POL.linePrice = newPrice }) <$> orderLine
  in
    replaceOrderLine (PO.lines order) orderLineId <$> newOrderLine

    
-- TODO is pipe operator (F#'s |>) idiomatic in Haskell? 
-- (apparently &, see https://stackoverflow.com/questions/1457140/haskell-composition-vs-fs-pipe-forward-operator)
-- if yes change where applicable
  
-- toShippingAddress :: UnvalidatedAddress -> ShippingAddress 
-- toShippingAddress str = undefined

-- toBillingAddress :: UnvalidatedAddress -> BillingAddress 
-- toBillingAddress str = undefined

-- createOrderAcknowledgmentLetter :: PricedOrder -> HTMLString
-- createOrderAcknowledgmentLetter = undefined

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

