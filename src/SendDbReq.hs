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

sendReq :: Text -> IO ReqResponse
sendReq sendStr = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (https "cedric.app" /: "api" /: "dbsync" /: "postgrest" /: "ma_tx_out")
            NoReqBody
            jsonResponse
            $ "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: Text)
              <> "order" =: ("id.desc" :: Text)
              <> "limit" =: (1 :: Int)
              <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: Text)
              <> "multi_asset.name" =: ("eq.\\x" <> sendStr :: Text)
      ) ::
      (FromJSON j) => IO (Either HttpException (JsonResponse j))

  case result of
    Left (VanillaHttpException e) -> do
      return $ InvalidResponse "\tNot a valid request - Vanilla"
    Left _ -> return $ InvalidResponse "\tNot a valid request - Json"
    Right v -> return $ ValidResponse (responseBody v :: Value)
