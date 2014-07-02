{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad
import Control.Exception
import Data.Char

default (Int, Double)

data (Show a) => Cell a = CellContent a
data (Cell a) => Grid a = Content [[a]]

instance (Show a) => Show (Grid a) where
   show (Content blar) =
     unlines $ map (\row -> (map intToDigit row)) blar


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getKeystroke :: IO Int
getKeystroke = do
  x <- withEcho False getChar
  return $ ord x

makeMap :: Int -> Grid Int
makeMap a = Content [[a | _ <- [0..(5 :: Int)]] | _ <- [0..5 :: Int]]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  let gamemap = makeMap 0
  putStrLn $ show gamemap

  --forever $ (getKeystroke >>= (\x -> return $ show x) >>= putStrLn)
