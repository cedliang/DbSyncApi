{-# LANGUAGE OverloadedStrings #-}

module GetTx
  ( getTx,
  )
where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson
import Data.Text.Lazy
import Network.HTTP.Req

getTx :: Text -> ExceptT Text IO Value
getTx addr = do
  result <-
    runExceptT $
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (http "127.0.0.1" /: "api" /: "v1" /: "tx" /~ addr)
            NoReqBody
            jsonResponse
            (port 5000)
      )

  either (const $ throwE "\tNot a valid txhash.") (return . responseBody) result
