{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad
import Control.Exception
import Data.Char

default (Int, Double)

data Cell a = CellContent a

data Grid a = Content [[ a ]]

showcell :: (Show a) => Cell a -> String
showcell (CellContent a) = show a

showmap :: (Show a) => Grid (Cell a) -> String
showmap (Content grid) = unlines $ map unwords $ map2 showcell grid

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 fn list = map (\row -> (map fn row)) list

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getKeystroke :: IO Int
getKeystroke = do
  x <- withEcho False getChar
  return $ ord x

makeMap :: Grid (Cell Char)
makeMap = Content [ [ CellContent 'a' | _ <- [0..(5 :: Int)] ] | _ <- [0..5 :: Int] ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  let gamemap = makeMap
  putStrLn $ showmap gamemap

  --forever $ (getKeystroke >>= (\x -> return $ show x) >>= putStrLn)
