module Lib (someFunc) where

newtype ProductCode = ProductCode String deriving (Show)
newtype OrderId = OrderId String deriving (Show)
newtype OrderLine = OrderLine String deriving (Show)

data OrderQuantity = UnitQuantity Int | KilogramQuantity Double deriving (Show)
data CardType = Visa | Master deriving (Show)

data CreditCardInfo = CreditCardInfo { 
  cardType :: CardType,
  cardNumber :: CardType
} deriving (Show)

data Order = Order {
  orderId :: OrderId,
  lines :: [OrderLine]
}

printQuantity qt =
  case qt of
    UnitQuantity q -> putStrLn $ show q
    KilogramQuantity q -> putStrLn $ show q

printList :: Show a => [a] -> String
printList list =
  case list of
    [] -> "empty!"
    [x] -> "one!" <> show x
    [x,y] -> "two!" <> show x <> ", " <> show y
    (x:xs) -> "other?? head: " <> show x <> "rest: " <> show xs

someFunc :: IO ()
someFunc = putStrLn $ show $ KilogramQuantity 2.4
