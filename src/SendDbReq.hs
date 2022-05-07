{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SendDbReq
  ( sendReq,
  )
where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (FromJSON, Value)
import Data.Text.Lazy (Text)
import Network.HTTP.Req

sendReq :: Option 'Https -> Text -> ExceptT Int IO Value
sendReq queryScheme searchTable = do
  result <-
    runExceptT $
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (https "cedric.app" /: "api" /: "dbsync" /: "postgrest" /~ searchTable)
            NoReqBody
            jsonResponse
            queryScheme
      )

  either (const $ throwE 400) (return . responseBody) result