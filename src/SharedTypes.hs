module SharedTypes where

import Data.Time

newtype OrderId = OrderId String deriving (Eq, Show)
newtype OrderLineId = OrderLineId String deriving (Eq, Show)
newtype ProductId = ProductId String deriving (Eq, Show)
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
-- newtype ValidatedShippingAddress = ValidatedShippingAddress String deriving Show
-- newtype ValidatedBillingAddress = ValidatedBillingAddress String deriving Show
-- newtype ValidatedOrderLine = ValidatedOrderLine String deriving Show

newtype CheckedAddress = CheckedAddress UnvalidatedAddress deriving Show
newtype AddressValidationError = AddressValidationError String deriving Show

newtype HTMLString = HTMLString String deriving Show

-- TODO
newtype AcknowledgmentSent = AcknowledgmentSent String
newtype EmailContactInfo = EmailContactInfo String deriving Show
newtype PostalContactInfo = PostalContactInfo String deriving Show
newtype CustomerInfo = CustomerInfo String deriving Show
newtype PricedOrder = PricedOrder String deriving Show

newtype PricingError = PricingError String deriving Show

data ProductCode = WidgetCode String | GizmoCode String deriving (Show)
data CardType = Visa | Master deriving (Show)

data SendResult = Sent | NotSent

data CustomerEmail = Unverfied EmailAddress | Verified VerifiedEmailAddress deriving Show


data ShippingAddress = ShippingAddress {
}

data BillingAddress = BillingAddress {
}

data Command a = Command {
  commandData :: a,
  timeStamp :: UTCTime,
  userId :: CustomerId
}

data ValidationError = ValidationError {
  fieldName :: String,
  errorDescription :: String
}
