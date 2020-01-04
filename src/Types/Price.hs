module Types.Price(
  Price(Price), unsafeCreate, multiply
) where

-- Constrained to be a decimal between 0.0 and 1000.00 
newtype Price = Price Double deriving (Show)

-- Create a Price from a decimal.
-- Throw an exception if out of bounds. This should only be used if you know the value is valid.
unsafeCreate :: Double -> Price
unsafeCreate value | value > 0 && value < 1000 = Price value
                   | otherwise = error "Invalid value"

multiply :: Double -> Price -> Price
multiply qty (Price p) = 
  unsafeCreate $ qty * p
