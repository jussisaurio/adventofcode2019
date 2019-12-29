{-# LANGUAGE OverloadedStrings #-}

module Day6 where
  import Control.Monad.State
  import qualified Data.Text as T
  import qualified Data.Map as M
  import Data.Maybe (isJust, fromJust)
  
  upsert :: Ord a => M.Map a [b] -> a -> b -> M.Map a [b]
  upsert m k v
    | not $ M.member k m = M.insert k [v] m
    | otherwise = M.adjust (v :) k m

  safeTuple :: String -> Maybe (T.Text, T.Text)
  safeTuple str
    | length res == 2 = let [a,b] = res in Just (a,b)
    | otherwise = Nothing
    where res = T.splitOn ")" $ T.pack str

  getInput = map fromJust . filter isJust . map safeTuple . lines <$> readFile "day6.txt"

  -- COM has zero orbits
  -- COM orbiting nodes have 1, the ones orbiting those have 2... etc
  
  type Bookkeep = M.Map T.Text [T.Text]
  type MyPair = (T.Text, T.Text)
  startState :: Bookkeep
  startState = M.empty

  orbits :: Bookkeep -> Integer -> [T.Text] -> Integer
  orbits m d [] = d
  orbits m d l = d + sum (map (\l2 -> orbits m (d+1) $ M.findWithDefault [] l2 m) l)


  loop :: [MyPair] -> State Bookkeep Integer
  loop [] = do
    mapping <- get
    let com = fromJust $ M.lookup "COM" mapping
    pure $ orbits mapping 0 com

  loop (p:ps) = do
    mapping <- get
    let (k, v) = p
    put $ upsert mapping k v
    loop ps

  part1 = do
    pairs <- getInput
    print $ evalState (loop pairs) startState
  

  type Bookkeep2 = M.Map T.Text T.Text
  startState2 :: Bookkeep2
  startState2 = M.empty

  backtrack :: Bookkeep2 -> T.Text -> [T.Text]
  backtrack m k = go2 m k []
    where
      go2 m' k' l =
        case M.lookup k' m' of
          Just val -> go2 m' val (val : l)
          Nothing -> l
  
  zipPad :: a -> [a] -> [a] -> [(a,a)]
  zipPad p (x:xs) (y:ys) = (x,y) : zipPad p xs ys
  zipPad p l1 [] = zip l1 (repeat p)
  zipPad p [] l2 = zip (repeat p) l2

  numNodes :: (T.Text,T.Text) -> Integer
  numNodes (a,b)
    | a == "" || b == "" = 1
    | otherwise = 2

  loop2 :: [MyPair] -> State Bookkeep2 Integer
  loop2 [] = do
    mapping <- get
    let prevNodes = backtrack mapping in pure $ sum $ map numNodes $ dropWhile (uncurry (==)) $ zipPad "" (prevNodes "YOU") (prevNodes "SAN")

  loop2 (p:ps) = do
    mapping <- get
    let (k, v) = p
    put $ M.insert k v mapping
    loop2 ps
  
  flipTuple :: (a,a) -> (a,a)
  flipTuple (k,v) = (v,k)

  part2 = do
    pairs <- map flipTuple <$> getInput
    print $ evalState (loop2 pairs) startState2
