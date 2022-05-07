{-# LANGUAGE OverloadedStrings #-}

module GetTx
  ( getTx,
  )
where

import Data.Aeson
import Data.Text.Lazy
import Network.HTTP.Req
import UnliftIO.Exception

getTx :: Text -> IO (Either Text Value)
getTx addr = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (http "127.0.0.1" /: "api" /: "v1" /: "tx" /~ addr)
            NoReqBody
            jsonResponse
            (port 5000)
      ) ::
      (FromJSON j) => IO (Either HttpException (JsonResponse j))

  case result of
    Left (VanillaHttpException _) -> return $ Left "\tNot a valid txhash."
    Left _ -> return $ Left "\tNot a valid txhash."
    Right v -> return $ Right (responseBody v :: Value)
