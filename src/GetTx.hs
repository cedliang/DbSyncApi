{-# LANGUAGE OverloadedStrings #-}

module GetTx
  ( getTx,
    AddrInfo (ValidHash, InvalidHash),
  )
where

import Data.Aeson
import Data.Text.Lazy
import Network.HTTP.Req
import UnliftIO.Exception

data AddrInfo
  = ValidHash Value
  | InvalidHash Text
  deriving (Eq, Show)

getTx :: Text -> IO AddrInfo
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
    Left (VanillaHttpException _) -> return $ InvalidHash "\tNot a valid txhash."
    Left _ -> return $ InvalidHash "\tNot a valid txhash."
    Right v -> return $ ValidHash (responseBody v :: Value)
