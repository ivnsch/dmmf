module Types.OrderAcknowledgment where

import SharedTypes
import Types.EmailAddress

data OrderAcknowledgment = OrderAcknowledgment {
  emailAddress :: EmailAddress,
  letter :: HTMLString
}
