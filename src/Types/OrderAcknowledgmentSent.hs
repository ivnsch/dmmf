module Types.OrderAcknowledgmentSent where

import SharedTypes
import Types.OrderId

data OrderAcknowledgmentSent = OrderAcknowledgmentSent {
  orderId :: OrderId,
  emailAddress :: EmailAddress
}
 
