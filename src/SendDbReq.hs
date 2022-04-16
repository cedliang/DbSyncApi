{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq,
    ReqResponse (ValidResponse, InvalidResponse),
  )
where

import Data.Aeson
import Data.Text.Lazy
import Network.HTTP.Req
import UnliftIO.Exception

data ReqResponse
  = ValidResponse Value
  | InvalidResponse Text
  deriving (Eq, Show)

sendReq :: Option 'Https -> Text -> IO ReqResponse
sendReq queryScheme searchTable = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (https "cedric.app" /: "api" /: "dbsync" /: "postgrest" /~ searchTable)
            NoReqBody
            jsonResponse
            queryScheme
      ) ::
      (FromJSON j) => IO (Either HttpException (JsonResponse j))

  case result of
    Left (VanillaHttpException e) -> return $ InvalidResponse "\tNot a valid request - Vanilla"
    Left _ -> return $ InvalidResponse "\tNot a valid request - Json"
    Right v -> return $ ValidResponse (responseBody v :: Value)
