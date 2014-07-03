{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import System.Random
import Control.Monad.State

default (Int, Double)

data Rect = R Int Int Int Int
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

rectContainsPoint :: Int -> Int -> Rect -> Bool
rectContainsPoint px py (R x y w h) =
  (px >= x) && (px <= (x + w)) && (py >= y) && (py <= (y + h))

makeRooms :: [ Rect ]
makeRooms = [ R 1 1 3 3, R 4 4 3 3 ]

getMapChar :: Int -> Int -> [ Rect ] -> Char
getMapChar x y rooms =
  if or (map (rectContainsPoint x y) rooms) then '_' else '#'

makeMap :: Int -> Grid (Cell Char)
makeMap size =
    Content (map2 CellContent mapWithRooms)
  where
    rooms = makeRooms
    mapWithRooms :: [[ Char ]] = [[(getMapChar x y rooms) | x <- [0..10]] | y <- [0..10]]

main :: IO ()
main = do
  --  g <- newStdGen

  hSetBuffering stdin NoBuffering

  let gamemap :: Grid (Cell Char) = makeMap 20
  putStrLn $ showmap gamemap

  --forever $ (getKeystroke >>= (\x -> return $ show x) >>= putStrLn)
