{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (servantIO) where

import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           GetHandle
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Types

defaultConfig :: Config
defaultConfig = Config { hostPort = 4000
                       , https = True
                       , serverHost = "https://cedric.app/api/dbsync/postgrest"
                       , serverPort = Nothing
                       }

-- defaultConfig :: Config
-- defaultConfig = Config { hostPort = 4000
--                        , https = False
--                        , serverHost = "http://192.168.20.10"
--                        , serverPort = Just 3000
--                        }
servantIO :: IO ()
servantIO = do
  run (hostPort defaultConfig)
    $ (simpleCors . logStdoutDev)
    $ servApp defaultConfig

servApp :: Config -> Application
servApp = serve (Proxy :: Proxy CardanoAPI) . server1

type CardanoAPI =
  "adahandle" :> Capture "inputHandle" TL.Text :> Get '[PlainText] TL.Text

server1 :: Config -> ServerT CardanoAPI Handler
server1 queryUrl = flip runReaderT queryUrl . handleHandler
