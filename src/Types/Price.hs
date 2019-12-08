module Types.Price(
  Price(Price), create, multiply
) where

newtype Price = Price Double deriving (Show)

create :: Double -> Price
create = Price

multiply :: Double -> Price -> Price
multiply qty (Price p) = 
  create $ qty * p 
