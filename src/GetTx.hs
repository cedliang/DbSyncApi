{-# LANGUAGE OverloadedStrings #-}

module GetTx
    (
        getTx,
        AddrInfo (ValidHash, InvalidHash)
    )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import Text.Read (readMaybe)
import Data.Aeson
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
