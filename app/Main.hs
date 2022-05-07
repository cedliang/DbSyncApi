{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO (getLine, putStrLn)
import GetHandle (getHandle)
import Server (mainScotty)

main :: IO ()
main = mainScotty

-- main :: IO ()
-- main = forever $ do
--   l <- Data.Text.Lazy.IO.getLine
--   h <- runExceptT $ getHandle l

--   let p = case h of
--         Right handle -> handle
--         Left sCode ->
--           let errStr = case sCode of
--                 400 -> "Bad request"
--                 404 -> "Handle not found"
--            in pack $ show sCode ++ ": " ++ errStr

--   Data.Text.Lazy.IO.putStrLn p
