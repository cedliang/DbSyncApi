{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq (sendReq) where

import           Control.Exception          (try)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.Text.Lazy             as TL
import           Network.HTTP.Req
import           Servant

sendReq :: Option 'Https -> TL.Text -> ReaderT (Url 'Https) Handler Value
sendReq queryScheme searchTable = do
  myUri <- ask
  result <- liftIO
              ( try $ runReq defaultHttpConfig $ do
                  req
                    Network.HTTP.Req.GET
                    (myUri /~ searchTable)
                    NoReqBody
                    jsonResponse
                    queryScheme
                :: IO (Either HttpException (JsonResponse Value))
              )
  either (const $ lift $ throwError $ err500 {errBody = "Internal server error"}) (pure . responseBody) result
