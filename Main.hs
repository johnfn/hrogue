{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import Data.Array
import System.Random
import Control.Monad.State

default (Int, Double)

data Cell a = CellContent a deriving Show
data Grid a = Content [[ a ]] deriving Show

showcell :: (Show a) => Cell a -> String
showcell (CellContent a) = take 1 $ drop 1 $ show a

showmap :: (Show a) => Grid (Cell a) -> String
showmap (Content grid) = unlines $ map (intercalate "") $ map2 showcell grid

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

{-
addRoom :: [[ Char ]] -> [[ Char ]]
addRoom grid =
    grid // [((x, y), '_') | x <- [roomX..roomX + roomW], y <- [roomY..roomY + roomH]]
  where
    roomX :: Int = 0
    roomY :: Int = 0
    roomW :: Int = 4
    roomH :: Int = 4
-}

makeMap :: Int -> Grid (Cell Char)
makeMap size =
    Content (map2 CellContent mapWithRooms)
  where
    adjustedSize :: Int = size - 1
    numRooms :: Int = 8
    emptyMap :: [[ Char ]] = [[ 'X' | _ <- [0..adjustedSize]] | _ <- [0..adjustedSize]]
    mapWithRooms :: [[ Char ]] = (iterate addRoom emptyMap) !! numRooms

main :: IO ()
main = do
  g <- newStdGen

  hSetBuffering stdin NoBuffering

  let gamemap :: Grid (Cell Char) = makeMap 20
  putStrLn $ showmap gamemap

  --forever $ (getKeystroke >>= (\x -> return $ show x) >>= putStrLn)
