module Types.SendOrderAcknowledgment where

import Types.OrderAcknowledgment
import SharedTypes

type SendOrderAcknowledgment = OrderAcknowledgment -> SendResult
