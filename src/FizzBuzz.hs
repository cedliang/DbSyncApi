{-# LANGUAGE OverloadedStrings #-}

module FizzBuzz (
    fizzBuzz
)
where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment ( getArgs )
import System.IO ()
import Text.Read ( readMaybe )
import TextShow ( TextShow(showt) )

-- main :: IO ()
-- main = do
--   argsList <- getArgs
--   case argsList of
--     [] -> T.putStrLn ("No arg provided" :: T.Text)
--     (arg : args) -> do
--       let n = readMaybe arg :: Maybe Int
--       case n of
--         Just val ->
--           do
--             let prtStr = T.unlines $ fizzBuzz val
--             T.putStrLn prtStr
--             when (("-w" :: T.Text) `elem` map T.pack args) $ T.writeFile "fb.txt" prtStr
--         _ -> T.putStrLn ("Arg is not integer" :: T.Text)

fizzMap = [(3, "Fizz"), (5, "Buzz")]

fizzBuzz :: Int -> [T.Text]
fizzBuzz n = map fizzBuzzConverter [0 .. n]
  where
    fizzMapFiltered :: Int -> [(Int, T.Text)]
    fizzMapFiltered i = filter (\tup -> i `mod` fst tup == 0) fizzMap

    fizzBuzzConverter :: Int -> T.Text
    fizzBuzzConverter i =
      let mapFiltered = fizzMapFiltered i
       in case mapFiltered of
            [] -> showt i
            _ -> foldr1 T.append $ map snd mapFiltered