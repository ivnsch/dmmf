{-# LANGUAGE DeriveGeneric #-}

module UnvalidatedOrderLine where

import SharedTypes
import qualified OrderId
import qualified CustomerInfo
import Data.Aeson
import GHC.Generics

data UnvalidatedOrderLine = UnvalidatedOrderLine {
  orderLineId :: String,
  orderId :: String,
  productCode :: String,
  quantity :: Double
} deriving (Show, Generic)

instance FromJSON UnvalidatedOrderLine
instance ToJSON UnvalidatedOrderLine
