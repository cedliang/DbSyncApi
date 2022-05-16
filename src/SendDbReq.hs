{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq,
  )
where

import Control.Exception (handle, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON, Value)
import Data.Text.Lazy (Text)
import Network.HTTP.Req
import Text.URI

sendReq :: Option 'Https -> Text -> ExceptT Int (ReaderT (Url 'Https) IO) Value
sendReq queryScheme searchTable = do
  myUri <- lift ask

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

  either (const $ throwE 500) (return . responseBody) result