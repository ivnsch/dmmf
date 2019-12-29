module Types.OrderAcknowledgmentSent where

import SharedTypes
import Types.OrderId
import Types.EmailAddress

data OrderAcknowledgmentSent = OrderAcknowledgmentSent {
  orderId :: OrderId,
  emailAddress :: EmailAddress
}
 
