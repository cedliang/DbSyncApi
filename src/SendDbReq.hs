{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq, ReqResponse (ValidResponse, InvalidResponse)
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
            (https "cedric.app" /: "api" /: "dbsync" /: "postgrest" /~ sendStr)
            NoReqBody
            jsonResponse
            mempty
      ) ::
      (FromJSON j) => IO (Either HttpException (JsonResponse j))

  case result of
    Left (VanillaHttpException _) -> return $ InvalidResponse "\tNot a valid request."
    Left _ -> return $ InvalidResponse "\tNot a valid request."
    Right v -> return $ ValidResponse (responseBody v :: Value)
