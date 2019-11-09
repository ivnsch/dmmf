module Lib (someFunc) where

import OrderTakingDomain

someFunc :: IO ()
someFunc = putStrLn $ show $ KilogramQuantity 2.4
