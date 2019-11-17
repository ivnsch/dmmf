module KilogramQuantity(
  KilogramQuantity, create, value
) where

newtype KilogramQuantity = KilogramQuantity Double deriving (Eq, Show)

create :: Double -> KilogramQuantity
create value | value > 0 && value < 1000 = KilogramQuantity value
             | otherwise = error "Invalid value"

value :: KilogramQuantity -> Double
value (KilogramQuantity value) = value
