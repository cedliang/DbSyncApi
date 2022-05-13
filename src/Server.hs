{-# LANGUAGE OverloadedStrings #-}

module Server
  ( mainScotty,
  )
where

import Control.Exception (catch)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 as B (pack)
import Data.Maybe (fromJust)
import Data.Text as T (Text, pack)
import Data.Text.Lazy as TL (Text, pack)
import GetHandle (getHandle)
import GetTx (getTx)
import Network.HTTP.Req (renderUrl, useHttpsURI, useURI)
import Network.HTTP.Types (mkStatus)
import Network.HTTP.Types.Method (StdMethod (GET))
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
import System.Exit
import Text.URI (ParseException (ParseException), URI, mkURI)
import Web.Scotty

data QueryProtocol = Http | Https deriving (Show, Eq)

data Config = Config
  { hostPort :: Int,
    serverUrl :: T.Text
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config {hostPort = 3000, serverUrl = "https://cedric.app/api/dbsync/postgrest"}

mainScotty :: IO ()
mainScotty = do
  serverUrl <- Control.Exception.catch (mkURI $ serverUrl defaultConfig) (const $ die "Invalid URL cannot be parsed" :: ParseException -> IO URI)

  scotty (hostPort defaultConfig) $ do
    middleware simpleCors
    middleware logStdoutDev

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
