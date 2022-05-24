{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server (servantIO) where

import qualified Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           GetHandle
import           Network.HTTP.Req
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           SendDbReq
import           Servant
import           System.Exit
import           Text.URI                             as TU

data Config = Config
  { hostPort   :: Int,
    serverUrl  :: T.Text,
    serverPort :: Maybe Int
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config {hostPort = 4000, serverUrl = "https://cedric.app/api/dbsync/postgrest", serverPort = Nothing}

-- defaultConfig :: Config
-- defaultConfig = Config {hostPort = 4000, serverUrl = "http://192.168.20.10/", serverPort = Just 3000}

getUrlFromConfig :: IO QueryServerConfig
getUrlFromConfig = do
  serverUri <- Control.Exception.catch (mkURI $ serverUrl defaultConfig) (const $ die "Invalid URL cannot be parsed" :: ParseException -> IO TU.URI)
  let serverP = serverPort defaultConfig
      rawServerUrl = useURI serverUri
  when (isNothing rawServerUrl) (die "Invalid URL cannot be parsed")
  case fromJust rawServerUrl of
    Left a  -> pure $ QueryServerConfig (Left $ fst a) serverP
    Right a -> pure $ QueryServerConfig (Right $ fst a) serverP

servantIO :: IO ()
servantIO = do
  queryUrl <- getUrlFromConfig
  run (hostPort defaultConfig) $ (simpleCors . logStdoutDev) $ servApp queryUrl

servApp :: QueryServerConfig -> Application
servApp = serve (Proxy :: Proxy CardanoAPI) . server1

type CardanoAPI = "adahandle" :> Capture "inputHandle" TL.Text :> Get '[PlainText] TL.Text

server1 :: QueryServerConfig -> ServerT CardanoAPI Handler
server1 queryUrl = flip runReaderT queryUrl . handleHandler
