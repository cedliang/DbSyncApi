{-# LANGUAGE OverloadedStrings #-}

module GetHandle
    (
        getHandle,
        HandleInfo (ValidHandle, InvalidHandle)
    )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import Text.Read (readMaybe)
import UnliftIO.Exception


data HandleInfo
  = ValidHandle Text
  | InvalidHandle Text
  deriving (Eq, Show)

getHandle :: Text -> IO HandleInfo
getHandle handleName = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (http "127.0.0.1" /: "api" /: "v1" /: "adahandle" /~ handleName)
            NoReqBody
            lbsResponse
            (port 5000)
      )

  case result of
    Left (VanillaHttpException _) -> return $ InvalidHandle "\tNot a valid handle."
    Left _ -> return $ InvalidHandle "\tNot a valid handle."
    Right bs -> return $ ValidHandle $ TL.Encoding.decodeUtf8 $ responseBody bs
