module Lib (someFunc) where

import OrderQuantity
import OrderTakingDomain

someFunc :: IO ()
someFunc = print $ KilogramQuantity 2.4
