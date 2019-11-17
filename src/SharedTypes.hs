module SharedTypes where

import Data.Time
import String50

newtype ProductId = ProductId String deriving (Eq, Show)
newtype CustomerId = CustomerId Int deriving (Show)
newtype InvoiceId = InvoiceId Int deriving (Show)
newtype ContactId = ContactId Int deriving (Show, Eq)
newtype EmailAddress = EmailAddress String deriving (Eq, Show)
newtype VerifiedEmailAddress = VerifiedEmailAddress String deriving (Show)
newtype PhoneNumber = PhoneNumber String deriving (Show)
newtype UnvalidatedAddress = UnvalidatedAddress String deriving Show
newtype ValidatedAddress = ValidatedAddress String deriving Show
-- newtype ValidatedShippingAddress = ValidatedShippingAddress String deriving Show
-- newtype ValidatedBillingAddress = ValidatedBillingAddress String deriving Show
-- newtype ValidatedOrderLine = ValidatedOrderLine String deriving Show

newtype CheckedAddress = CheckedAddress UnvalidatedAddress deriving Show
newtype AddressValidationError = AddressValidationError String deriving Show

newtype HTMLString = HTMLString String deriving Show

newtype City = City String50 deriving (Eq, Show)
newtype ZipCode = ZipCode String deriving (Eq, Show)

-- TODO
newtype EmailContactInfo = EmailContactInfo String deriving Show
newtype PostalContactInfo = PostalContactInfo String deriving Show

newtype PricingError = PricingError String deriving Show

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
