{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day13 where
  import Intcode
  import qualified FsHelpers as Fsh
  import qualified Data.Sequence as S
  import qualified Data.Map as M
  import Control.Monad
  import Control.Monad.Trans.State
  import Control.Monad.IO.Class
  import Data.Maybe (isJust, fromMaybe)
  import Data.List

  type Tiles = M.Map Coord Tile
  type Coord = (Integer, Integer)



  data Tile = Empty | Wall | Block | Paddle | Ball | Score Integer deriving (Eq, Show)

  getRows ::  Coord -> [[Coord]] -> [[Coord]]
  getRows c acc = if null acc then [[c]] else let x:xs = acc in if snd c == snd (head x) then (c:x):xs else [c] : acc
  
  getTileAndCoord :: [Integer] -> (Tile, Coord)
  getTileAndCoord lst = if length lst /= 3
    then error "invalid program"
    else (tile, (c,b))
         where
          [a,b,c] = lst
          tile = case a of
                    0 -> Empty
                    1 -> Wall
                    2 -> Block
                    3 -> Paddle
                    4 -> Ball
                    x -> Score x

  group' :: Int -> [a] -> [[a]]
  group' _ [] = []
  group' n l
    | n > 0 = take n l : group' n (drop n l)
    | otherwise = error "Negative or zero n"

  blockCount :: [Tile] -> Integer
  blockCount = fromIntegral . length . filter (==Block)

  part1 = do
    codes <- Fsh.getInputAsInts "day13.txt"
    let i = initS { program = codes, input = [] }
    pure $ blockCount . map (fst . getTileAndCoord) $ group' 3 (map fromIntegral $ output (loop i))

  getScore :: Tile -> Maybe Integer
  getScore (Score a) = Just a
  getScore _ = Nothing

  printTile :: Tiles -> Coord -> Char
  printTile tiles c = fromMaybe '!' (M.lookup c tiles >>= \case
                                                      Empty -> Just ' '
                                                      Wall -> Just 'W'
                                                      Block -> Just 'B'
                                                      Paddle -> Just '='
                                                      Ball -> Just 'o'
                                                      Score a -> Just 'S')

  getJoystick :: Char -> Integer
  getJoystick 'a' = -1
  getJoystick 's' = 0
  getJoystick 'd' = 1
  getJoystick _ = 0
  
  gameLoop :: [(IntcodeState, Tiles)] -> IO (Maybe Tile)
  gameLoop icsTiles = do
    let (ics, tiles) = head icsTiles
    let newIcs = loop ics
    let outputTiles = map getTileAndCoord $ group' 3 (map fromIntegral $ output newIcs)

    let updatedTiles = foldr (\(t,c) acc -> M.insert c t acc) tiles outputTiles
    let tcs = sortOn snd $ M.keys updatedTiles
    let rows = map (map (printTile updatedTiles)) $ foldr getRows [] tcs
    mapM_ print rows
    case blockCount (M.elems updatedTiles) of
      0 -> return $ find (isJust . getScore) (M.elems updatedTiles)
      _ -> getChar >>= \c -> if c == 'r' then gameLoop (drop 1 icsTiles) else gameLoop $ (newIcs { output = [], input = [getJoystick c] }, updatedTiles) : icsTiles

    

  part2 = do
    codes <- Fsh.getInputAsInts "day13.txt"
    let i = initS { program = S.update 0 2 codes, input = [0] }
    gameLoop [(i, M.empty)]