module OrderAcknowledgmentSent where

import SharedTypes

data OrderAcknowledgmentSent = OrderAcknowledgmentSent {
  orderId :: OrderId,
  emailAddress :: EmailAddress
}
 