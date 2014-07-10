{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Exception
import Data.Char
import Data.List
import Control.Monad
import System.Random
import Control.Monad.State

default (Int, Double)

data Rect = Rect Int Int Int Int
data Cell a = CellContent a deriving Show
data Grid a = Content [[ a ]] deriving Show
data Player = Player Int Int

data Game = Game (Grid (Cell Char)) Player

shownoquotes :: (Show a) => a -> String
shownoquotes a = take 1 $ drop 1 $ show a

showcell :: (Show a) => Cell a -> String
showcell (CellContent a) = shownoquotes a

show2 :: (Show a) => [[ a ]] -> String
show2 grid = unlines $ map (intercalate "") (map (map shownoquotes) grid)

extractCell :: Cell a -> a
extractCell (CellContent a) = a

showmap :: (Show a) => Grid (Cell a) -> String
showmap (Content grid) = show2 $ map2 showcell grid

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 fn list = map (\row -> (map fn row)) list

get2 :: [[a]] -> Int -> Int -> a
get2 grid i j = (grid !! i) !! j

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getKeystroke :: IO Int
getKeystroke = do
  x <- withEcho False getChar
  return $ ord x

rectContainsPoint :: Int -> Int -> Rect -> Bool
rectContainsPoint px py (Rect x y w h) =
  (px >= x) && (px <= (x + w)) && (py >= y) && (py <= (y + h))

makeRooms :: [ Rect ]
makeRooms = [ Rect 1 1 3 3, Rect 4 4 3 3 ]

getMapChar :: Int -> Int -> [ Rect ] -> Char
getMapChar x y rooms =
  if or (map (rectContainsPoint x y) rooms) then '_' else '#'

iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = f a >>= iterateM f

makeMap :: Int -> Grid (Cell Char)
makeMap size =
    Content (map2 CellContent mapWithRooms)
  where
    rooms = makeRooms
    mapWithRooms :: [[ Char ]] = [[(getMapChar x y rooms) | x <- [0..10]] | y <- [0..10]]

makePlayer :: Player
makePlayer = Player 4 4

makeGame :: Game
makeGame = Game (makeMap 10) makePlayer

renderGame :: Game -> String
renderGame (Game (Content mapList) (Player px py)) =
    show2 [[(renderChar x y) | x <- [0..10]] | y <- [0..10]]
  where
    bareMap = map2 extractCell mapList
    renderChar x y = if (px == x && py == y) then '@' else get2 bareMap x y


gameLoop :: Int -> IO ()
gameLoop state = do
    print state
    key <- getKeystroke
    gameLoop (updateState state key)
  where
    updateState state 119 {-W-}= state + 1
    updateState state 97  {-A-}= state - 1

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  let game = makeGame
  print $ renderGame game
  --putStrLn $ showmap gamemap

  {- WASD
  119
  97
  115
  100 
  -}

  gameLoop 0
