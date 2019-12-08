module Types.UnvalidatedCustomerInfo where

data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo {
  firstName :: String,
  lastName :: String,
  emailAddress :: String
}
