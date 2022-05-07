{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( mainScotty,
  )
where

import qualified Data.ByteString.Char8 as B
import GetHandle (getHandle)
import GetTx (getTx)
import Network.HTTP.Types (mkStatus)
import Network.HTTP.Types.Method
import Network.Wai.Middleware.Cors
import Web.Scotty
import Data.Text.Lazy (pack)

mainScotty :: IO ()
mainScotty = scotty 3000 $ do
  middleware simpleCors

  addroute Network.HTTP.Types.Method.GET "/tx/:txhash" $ do
    inputHash <- param "txhash"
    v <- liftAndCatchIO $ getTx inputHash
    case v of
      Right val -> Web.Scotty.json val
      Left errStr -> do
        status $ mkStatus 404 $ B.pack "Tx not found."
        text errStr

  addroute Network.HTTP.Types.Method.GET "/adahandle/:handle" $ do
    inputHandle <- param "handle"
    thandle <- liftAndCatchIO $ getHandle inputHandle
    case thandle of
      Right handle -> Web.Scotty.text handle
      Left sCode -> do
        let errStr = case sCode of
              400 -> "Bad request"
              404 -> "Handle not found"
        status $ mkStatus sCode $ B.pack errStr
        text $ pack $ show sCode ++ ": " ++ errStr
