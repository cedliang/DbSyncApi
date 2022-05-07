{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO (getLine, putStrLn)
import GetHandle
import Lib

main :: IO ()
main = mainScotty

-- main :: IO ()
-- main = forever $ do
--   l <- Data.Text.Lazy.IO.getLine
--   h <- getHandle l

--   let p = case h of
--         ValidHandle handle -> handle
--         InvalidHandle errStr -> errStr

--   Data.Text.Lazy.IO.putStrLn p
