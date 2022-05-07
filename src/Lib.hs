{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( mainScotty,
  )
where

import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Char8 as B (pack)
import Data.Text.Lazy as TL (pack)
import GetHandle (getHandle)
import GetTx (getTx)
import Network.HTTP.Types (mkStatus)
import Network.HTTP.Types.Method (StdMethod (GET))
import Network.Wai.Middleware.Cors (simpleCors)
import Web.Scotty

mainScotty :: IO ()
mainScotty = scotty 3000 $ do
  middleware simpleCors

  addroute Network.HTTP.Types.Method.GET "/tx/:txhash" $ do
    inputHash <- param "txhash"
    v <- liftAndCatchIO $ runExceptT $ getTx inputHash
    case v of
      Right val -> Web.Scotty.json val
      Left errStr -> do
        status $ mkStatus 404 $ B.pack "Tx not found."
        text errStr

  addroute Network.HTTP.Types.Method.GET "/adahandle/:handle" $ do
    inputHandle <- param "handle"
    thandle <- liftAndCatchIO $ runExceptT $ getHandle inputHandle
    case thandle of
      Right handle -> Web.Scotty.text handle
      Left sCode -> do
        let errStr = case sCode of
              400 -> "Bad request"
              404 -> "Handle not found"
        status $ mkStatus sCode $ B.pack errStr
        text $ TL.pack $ show sCode ++ ": " ++ errStr
