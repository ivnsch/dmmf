{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import OrderQuantity
import OrderTakingDomain
import qualified KilogramQuantity
import Data.Aeson

import UnvalidatedOrderLine

someFunc :: IO ()
someFunc =
  let
    str = "{\"orderLineId\" : \"1\", \"orderId\" : \"2\", \"productCode\" : \"3\",\"quantity\": 1}"
    uol = eitherDecode str :: Either String UnvalidatedOrderLine
    newstr = encode <$> uol
  in
    print newstr
-- someFunc = print $ KilogramQuantity.create 2.4


