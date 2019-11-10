-- TODO "smart constructors" - book loc 2618 / https://wiki.haskell.org/Smart_constructors
-- This seems cumbersome. We can't pattern match anymore on OrderQuantity. Solutions?
module OrderQuantity(
  OrderQuantity(UnitQuantity, KilogramQuantity)
  -- unitQuantity 
) where

-- data OrderQuantity = OrderUnitQuantity UnitQuantity UnitQuantityValue | OrderKilogramQuantity Double deriving (Show)
data OrderQuantity = UnitQuantity UnitQuantityValue | KilogramQuantity Double deriving (Show)

-- newtype UnitQuantity = UnitQuantity UnitQuantityValue deriving Show
type UnitQuantityValue = Int

-- unitQuantity :: UnitQuantityValue -> Either String UnitQuantity
-- unitQuantity value | value > 0 && value < 1000 = Right $ UnitQuantity value
--                    | otherwise = Left "Invalid value"

-- quantityValue :: UnitQuantity -> UnitQuantityValue
-- quantityValue (UnitQuantity value) = value
