module SharedTypes where

import Data.Time
import Types.String50
import Types.EmailAddress

-- ===============================
-- Simple types and constrained types related to the OrderTaking domain.
--
-- Note: In Haskell we need to put several in separate files/modules to avoid name clashes
-- ===============================

newtype ProductId = ProductId String deriving (Eq, Show)
newtype CustomerId = CustomerId Int deriving (Show)
newtype InvoiceId = InvoiceId Int deriving (Show)
newtype ContactId = ContactId Int deriving (Show, Eq)
newtype VerifiedEmailAddress = VerifiedEmailAddress String deriving (Show)
newtype PhoneNumber = PhoneNumber String deriving (Show)
newtype UnvalidatedAddress = UnvalidatedAddress String deriving Show
newtype ValidatedAddress = ValidatedAddress String deriving Show

newtype CheckedAddress = CheckedAddress UnvalidatedAddress deriving Show

newtype HTMLString = HTMLString String deriving Show

newtype City = City String50 deriving (Eq, Show)

-- A zip code
newtype ZipCode = ZipCode String deriving (Eq, Show)

newtype EmailContactInfo = EmailContactInfo String deriving Show
newtype PostalContactInfo = PostalContactInfo String deriving Show

data CardType = Visa | Master deriving (Show)

data SendResult = Sent | NotSent

data CustomerEmail = Unverfied EmailAddress | Verified VerifiedEmailAddress deriving Show

data Command a = Command {
  commandData :: a,
  timeStamp :: UTCTime,
  userId :: CustomerId
}

data ValidationError = ValidationError {
  fieldName :: String,
  errorDescription :: String
} deriving (Eq, Show)

newtype PricingError = PricingError String deriving (Eq, Show)
newtype RemoteServiceError = RemoteServiceError String deriving (Eq, Show)
data PlaceOrderError = Validation ValidationError | Pricing PricingError | RemoteService RemoteServiceError deriving (Eq, Show)

-- NOTE (port): Not using the F# repo's ConstrainedType, since this doesn't seem idiomatic in Haskell.
-- May be missing something but it seems an arbitrary constructor can be passed and this is not type safe.
