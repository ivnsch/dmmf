module Lib (someFunc) where

import OrderQuantity
import OrderTakingDomain
import qualified KilogramQuantity

someFunc :: IO ()
someFunc = print $ KilogramQuantity.create 2.4
