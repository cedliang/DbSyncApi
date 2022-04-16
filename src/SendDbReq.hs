{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq,
    ReqResponse (ValidResponse, InvalidResponse),
  )
where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import Text.Read (readMaybe)
import UnliftIO.Exception

data ReqResponse
  = ValidResponse Value
  | InvalidResponse Text
  deriving (Eq, Show)

sendReq :: Option 'Https -> IO ReqResponse
sendReq queryScheme = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (https "cedric.app" /: "api" /: "dbsync" /: "postgrest" /: "ma_tx_out")
            NoReqBody
            jsonResponse
            queryScheme
      ) ::
      (FromJSON j) => IO (Either HttpException (JsonResponse j))

  case result of
    Left (VanillaHttpException e) -> do
      return $ InvalidResponse "\tNot a valid request - Vanilla"
    Left _ -> return $ InvalidResponse "\tNot a valid request - Json"
    Right v -> return $ ValidResponse (responseBody v :: Value)
