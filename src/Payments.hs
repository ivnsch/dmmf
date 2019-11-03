module Payments where

  newtype CheckNumber = CheckNumber Int deriving (Show)
  newtype CardNumber = CardNumber String deriving (Show)
  newtype PaymentAmount = PaymentAmount Double

  data PaymentMethod = Cash | Check CheckNumber | Card CardNumber
  data Currency = EUR | USD

  data Payment = Payment { 
    amount :: PaymentAmount,
    currency :: Currency,
    method :: PaymentMethod
  }
