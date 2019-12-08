module Types.BillingAmount(
  BillingAmount(BillingAmount), create, value
) where

newtype BillingAmount = BillingAmount Double deriving (Show)

create :: Double -> BillingAmount
create = BillingAmount

value :: BillingAmount -> Double
value (BillingAmount value) = value
