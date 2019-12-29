module Types.KilogramQuantity(
  KilogramQuantity, create, value
) where

-- Constrained to be a decimal between 0.05 and 100.00 
newtype KilogramQuantity = KilogramQuantity Double deriving (Eq, Show)

create :: Double -> KilogramQuantity
create value | value > 0.05 && value < 100 = KilogramQuantity value
             | otherwise = error "Invalid value"

value :: KilogramQuantity -> Double
value (KilogramQuantity value) = value
