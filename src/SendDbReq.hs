{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON, Value)
import Data.Text.Lazy (Text)
import Network.HTTP.Req
import Network.HTTP.Req (useHttpsURI)
import Text.URI

sendReq :: Option 'Https -> Text -> ExceptT Int (ReaderT (Url 'Https) IO) Value
sendReq queryScheme searchTable = do
  myUri <- lift $ ask

  result <-
    runExceptT $
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (myUri /~ searchTable)
            NoReqBody
            jsonResponse
            queryScheme
      )

  either (const $ throwE 400) (return . responseBody) result