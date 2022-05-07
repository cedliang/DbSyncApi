{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq,
  )
where

import Data.Aeson
import Data.Text.Lazy
import Network.HTTP.Req
import UnliftIO.Exception

sendReq :: Option 'Https -> Text -> IO (Either Int Value)
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
    Left (VanillaHttpException e) -> return $ Left 400
    Left _ -> return $ Left 400
    Right v -> return $ Right (responseBody v :: Value)
