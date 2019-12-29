module Types.BillingAmount(
  BillingAmount(BillingAmount), create, value
) where

-- Constrained to be a decimal between 0.0 and 10000.00 
newtype BillingAmount = BillingAmount Double deriving (Show)

create :: Double -> BillingAmount
create value | value > 0 && value < 10000 = BillingAmount value
             | otherwise = error "Invalid value"

value :: BillingAmount -> Double
value (BillingAmount value) = value
