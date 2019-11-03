module Lib
    ( someFunc
    ) where

newtype ProductCode = ProductCode String deriving (Show)

data OrderQuantity = UnitQuantity Int | KilogramQuantity Double deriving (Show)

printQuantity qt =
  case qt of
    UnitQuantity q -> putStrLn $ show q
    KilogramQuantity q -> putStrLn $ show q



newtype OrderId = OrderId String deriving (Show)
newtype OrderLine = OrderLine String deriving (Show)

data CardType = Visa | Master deriving (Show)

data CreditCardInfo = CreditCardInfo { 
  cardType :: CardType,
  cardNumber :: CardType
} deriving (Show)


data Order = Order {
  orderId :: OrderId,
  lines :: [OrderLine]
}

printList :: Show a => [a] -> String
printList list =
  case list of
    [] -> "empty!"
    [x] -> "one!" <> show x
    [x,y] -> "two!" <> show x <> ", " <> show y
    (x:xs) -> "other?? head: " <> show x <> "rest: " <> show xs

someFunc :: IO ()
-- someFunc = putStrLn $ show $ KilogramQuantity 2.4
-- someFunc = printQuantity $ KilogramQuantity 2
someFunc = putStrLn $ printList [1,2,4] 

