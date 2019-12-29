module Types.Price(
  Price(Price), create, multiply
) where

-- Constrained to be a decimal between 0.0 and 1000.00 
newtype Price = Price Double deriving (Show)

create :: Double -> Price
create value | value > 0 && value < 1000 = Price value
             | otherwise = error "Invalid value"

multiply :: Double -> Price -> Price
multiply qty (Price p) = 
  create $ qty * p 
