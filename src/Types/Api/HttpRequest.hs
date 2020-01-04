module Types.Api.HttpRequest where

import SharedTypes

-- Very simplified version!
data HttpRequest = HttpRequest {
  action :: String,
  uri :: String,
  body :: JsonString
}

