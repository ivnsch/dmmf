{-# LANGUAGE DeriveGeneric #-}

module Types.UnvalidatedOrderLine where

import SharedTypes
import qualified Types.OrderId as OrderId
import qualified Types.CustomerInfo as CustomerInfo
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
