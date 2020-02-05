{-# LANGUAGE DeriveGeneric #-}

module Types.UnvalidatedOrderLine where

import Data.Aeson
import GHC.Generics

data UnvalidatedOrderLine = UnvalidatedOrderLine {
  orderLineId :: String,
  productCode :: String,
  quantity :: Double
} deriving (Show, Generic)

instance FromJSON UnvalidatedOrderLine
instance ToJSON UnvalidatedOrderLine
