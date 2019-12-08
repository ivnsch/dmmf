module Types.OrderAcknowledgment where

import SharedTypes

data OrderAcknowledgment = OrderAcknowledgment {
  emailAddress :: EmailAddress,
  letter :: HTMLString
}
