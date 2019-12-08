module Types.DTO.OrderLineDto where

data OrderLineDto = OrderLineDto {
  orderLineId :: Int,
  productCode :: String,
  quantity :: Maybe Int,
  description :: String
} deriving (Eq, Show)
