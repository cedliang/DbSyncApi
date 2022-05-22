{-# LANGUAGE OverloadedStrings #-}

module GetTx
  ( getTx,
  )
where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.Text.Lazy             as TL
import           Network.HTTP.Req

getTx :: TL.Text -> ExceptT TL.Text IO Value
getTx addr = do
  result <- liftIO
              ( try $ runReq defaultHttpConfig $ do
                  req
                    Network.HTTP.Req.GET
                    (http "127.0.0.1" /: "api" /: "v1" /: "tx" /~ addr)
                    NoReqBody
                    jsonResponse
                    (port 5000)
                :: IO (Either HttpException (JsonResponse Value))
              )
  either (const $ throwE "\tNot a valid txhash.") (pure . responseBody) result
