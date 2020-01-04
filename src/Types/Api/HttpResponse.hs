module Types.Api.HttpResponse where

import SharedTypes

-- Very simplified version!
data HttpResponse = HttpResponse {
  httpStatusCode :: Int,
  body :: JsonString
}


