module OrderAcknowledgmentSent where

import SharedTypes
import qualified OrderId

data OrderAcknowledgmentSent = OrderAcknowledgmentSent {
  orderId :: OrderId.OrderId,
  emailAddress :: EmailAddress
}
 