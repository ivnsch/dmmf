module Lib (someFunc) where

import OrderQuantity
import OrderTakingDomain

someFunc :: IO ()
someFunc = putStrLn $ show $ KilogramQuantity 2.4
