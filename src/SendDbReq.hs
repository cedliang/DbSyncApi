{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq (sendReq, QueryServerConfig (QueryServerConfig)) where

import           Control.Exception      (try)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text.Lazy         as TL
import           Network.HTTP.Req
import           Servant

data QueryServerConfig = QueryServerConfig
  { qServerUrl  :: Either (Url 'Http) (Url 'Https),
    qServerPort :: Maybe Int
  }

sendReq :: (Option 'Http, Option 'Https) -> TL.Text -> ReaderT QueryServerConfig Handler Value
sendReq queryScheme searchTable = do
  QueryServerConfig myUri qPort <- ask
  let portScheme = maybe mempty port qPort
  result <- case myUri of
    Left uri  -> liftIO $ httpReq uri portScheme
    Right uri -> liftIO $ httpsReq uri portScheme
  either (const $ lift $ throwError $ err500 {errBody = "Internal server error"}) (pure . responseBody) result
  where
    httpReq :: Url 'Http -> Option 'Http -> IO (Either HttpException (JsonResponse Value))
    httpReq uri portScheme =
      try $
        runReq defaultHttpConfig $
          req
            Network.HTTP.Req.GET
            (uri /~ searchTable)
            NoReqBody
            jsonResponse
            (fst queryScheme <> portScheme)

    httpsReq :: Url 'Https -> Option 'Https -> IO (Either HttpException (JsonResponse Value))
    httpsReq uri portScheme =
      try $
        runReq defaultHttpConfig $
          req
            Network.HTTP.Req.GET
            (uri /~ searchTable)
            NoReqBody
            jsonResponse
            (snd queryScheme <> portScheme)
