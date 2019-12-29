{-# LANGUAGE OverloadedStrings #-}

module Day3 where
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import qualified Data.Map.Strict as M
  import Data.Maybe (fromMaybe, isJust, fromJust)
  import Data.List (find)
  import Text.Read (readMaybe)
  import Parser

  data Direction = U | D | L | R deriving (Eq, Show)
  data Move = Move { direction :: Direction, distance :: Integer } deriving (Eq, Show)

  getInputAsLines :: String -> IO [String]
  getInputAsLines fn = lines <$> readFile fn

  directionParser :: Parser Direction
  directionParser = Parser func
    where
      func [] = Nothing
      func (x:xs)
        | x == 'U' = Just(U, xs)
        | x == 'D' = Just(D, xs)
        | x == 'L' = Just(L, xs)
        | x == 'R' = Just(R, xs)
        | otherwise = Nothing

  moveParser :: Parser Move
  moveParser = Move <$> directionParser <*> posIntParser <* zeroOrMore (char ',')

  parseMoves :: String -> Maybe [Move]
  parseMoves input = fst <$> runParser (zeroOrMore moveParser) input
  
  type Point = (Integer, Integer)
  manhattanDistance :: Point -> Point -> Integer
  manhattanDistance p1 p2 = abs(fst p1 - fst p2) + abs(snd p1 - snd p2)

  data Line = Line { start :: Point, end :: Point, startSteps :: Integer } deriving (Eq, Show)
  lineIsVertical :: Line -> Bool
  lineIsVertical l = fst (start l) == fst (end l)
  lineIntersection :: Line -> Line -> Maybe Point
  lineIntersection l1 l2
      | lineIsVertical l1 == lineIsVertical l2 = Nothing
      | lineIsVertical l1 = if x > leftBound && x < rightBound && y > topBound && y < bottomBound then Just (x,y) else Nothing
      | otherwise = lineIntersection l2 l1
        where
          x = fst(start l1)
          y = snd(start l2)
          leftBound = min (fst $ start l2) (fst $ end l2)
          rightBound = max (fst $ start l2) (fst $ end l2)
          topBound = min (snd $ start l1) (snd $ end l1)
          bottomBound = max (snd $ start l1) (snd $ end l1)

  lineIntersectionWithLines :: Line -> Line -> Maybe (Line, Line, Point)
  lineIntersectionWithLines l1 l2 = case lineIntersection l1 l2 of
    Just is -> Just (l1, l2, is)
    Nothing -> Nothing
  
  moveToLine :: Move -> Point -> Integer -> Line
  moveToLine move startPos startSteps =
    case direction move of
      U -> Line startPos (fst startPos, snd startPos - dist) startSteps
      D -> Line startPos (fst startPos, snd startPos + dist) startSteps
      L -> Line startPos (fst startPos - dist, snd startPos) startSteps
      R -> Line startPos (fst startPos + dist, snd startPos) startSteps
      where dist = distance move

  movesToLines :: [Move] -> [Line]
  movesToLines [] = []
  movesToLines moves = go moves (0,0) 0
      where
        go [] _ _ = []
        go (m:ms) pos steps = line : go ms nextPos nextSteps
          where
            line = moveToLine m pos steps
            nextPos = end line
            nextSteps = steps + distance m
  
  part1 :: [Line] -> [Line] -> Integer
  part1 l1 l2 = closestIntersection
        where closestIntersection = minimum $ map (manhattanDistance (0,0)) $ concatMap (\a -> map fromJust $ filter isJust $ map(`lineIntersection` a) l1) l2
  
  distanceTravelledToIntersection :: (Line, Line, Point) -> Integer
  distanceTravelledToIntersection (l1, l2, is) = startSteps l1 + manhattanDistance (start l1) is + startSteps l2 + manhattanDistance (start l2) is

  part2 :: [Line] -> [Line] -> Integer  
  part2 l1 l2 = cheapestIntersection
        where cheapestIntersection = minimum $ map distanceTravelledToIntersection $ concatMap (\a -> map fromJust $ filter isJust $ map(`lineIntersectionWithLines` a) l1) l2        
  
  main = do
    [first, second] <- getInputAsLines "day3.txt"
    let wire1Lines = movesToLines <$> parseMoves first 
    let wire2Lines = movesToLines <$> parseMoves second
    case (wire1Lines, wire2Lines) of
      (Nothing, _) -> pure ()
      (_, Nothing) -> pure ()
      (Just l1, Just l2) -> print $ part2 l1 l2