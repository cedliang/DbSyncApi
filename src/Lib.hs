{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( mainScotty,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import Network.HTTP.Types.Method
import Network.Wai.Middleware.Cors
import Text.Read (readMaybe)
import UnliftIO.Exception
import Web.Scotty
import GetHandle ( getHandle, HandleInfo (ValidHandle, InvalidHandle))
import GetTx ( getTx, AddrInfo (ValidHash, InvalidHash))

mainScotty :: IO ()
mainScotty = scotty 3000 $ do
  middleware simpleCors

  addroute Network.HTTP.Types.Method.GET "/tx/:txhash" $ do
    inputHash <- param "txhash"
    v <- liftAndCatchIO $ getTx inputHash
    case v of
      ValidHash val -> Web.Scotty.json val
      InvalidHash errStr -> do
        status $ mkStatus 404 $ B.pack "Tx not found."
        text errStr

  addroute Network.HTTP.Types.Method.GET "/adahandle/:handle" $ do
    inputHandle <- param "handle"
    thandle <- liftAndCatchIO $ getHandle inputHandle
    case thandle of
      ValidHandle handle -> Web.Scotty.text handle
      InvalidHandle errStr -> do
        status $ mkStatus 404 $ B.pack "Handle not found."
        text errStr

