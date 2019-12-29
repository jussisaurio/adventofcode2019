{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
module Day10 where
    import qualified Data.Text as T
    import Data.Sequence(ViewL ( (:<) ))
    import qualified Data.Sequence as S
    import Control.Monad.State
    import qualified Data.Map as M
    import qualified Data.Set as Set
    import Data.List
    import Data.Maybe (fromJust)

    getInput :: String -> IO [String]
    getInput fln = do
        f <- readFile fln
        return $ lines f

    type Coord = (Integer, Integer)
    type Vec = (Integer, Integer)
    data Loc = Asteroid Coord | Void deriving (Eq, Show, Ord)
    type WorldMap = M.Map Coord Loc
  
    mapCharToLoc :: Char -> Integer -> Loc
    mapCharToLoc '.' _ = Void
    mapCharToLoc '#' a = Asteroid (a `mod` (maxX + 1), a `div` (maxY + 1))
    mapCharToLoc _ _ = error "invalid input"

    populateMap :: String -> State ([Loc], Integer) [Loc]
    populateMap [] = do
        (w, _) <- get
        pure w
    populateMap (x:xs) = do
        (wmap, astNum) <- get
        case mapCharToLoc x astNum of
            Void -> put (wmap ++ [Void], astNum + 1) >> populateMap xs
            Asteroid crd -> put (wmap ++ [Asteroid crd], astNum + 1) >> populateMap xs
  
    buildMapRow :: Integer -> S.Seq Loc -> WorldMap -> WorldMap
    buildMapRow _ (S.viewl -> S.EmptyL) m = m
    buildMapRow n seq m = go seq 0 m
            where
                go (S.viewl -> S.EmptyL) _ mappi = mappi
                go (S.viewl -> x :< xs) i mappi = go xs (i+1) $ M.insert (i, n) x mappi

    something :: WorldMap -> (S.Seq Loc, Integer) -> WorldMap
    something m (row, y) = buildMapRow y row m

    castRay :: Coord -> WorldMap -> Vec -> Loc
    castRay (x, y) wm (dx, dy) = if (dx,dy) == (0,0) then Void else case M.lookup (x+dx, y+dy) wm of
        Nothing -> Void
        Just loc -> case loc of
            Void -> castRay (x+dx, y+dy) wm (dx, dy)
            ast -> ast

    mapScan :: WorldMap -> Coord -> Set.Set Loc
    mapScan wm coord = case M.lookup coord wm of
        Nothing -> Set.empty
        Just Void -> Set.empty
        _ -> Set.fromList . filter (/= Void) . map (castRay coord wm) $ allDirections coord
        
    divByAbsGcd :: (Integer, Integer) -> (Integer, Integer)
    divByAbsGcd (a,b) = let g = gcd (abs a) (abs b) in case (a,b,g) of
        (0, 0, _) -> (0,0)
        (0, b, _) -> (0, b `div` abs b)
        (a, 0, _) -> (a `div` abs a,0)
        (a, b, 0) -> (a,b)
        (a, b, g) -> (a `div` g, b `div` g)

    -- point directly above is 0 deg, directly to right is 90, directly down is 180... etc
    -- y1-y2 because in atk coordinates lower y = higher
    angle2P :: (Integral a, RealFloat b) => (a, a) -> (a, a) -> b
    angle2P (x1,y1) (x2,y2) = let ang = atan2 (fromIntegral $ x2-x1) (fromIntegral $ y1-y2) * 180 / pi in if ang < 0 then 360 + ang else ang

    allDirections :: Coord -> [Vec]
    allDirections (x1,y1) = Set.toList . Set.fromList $ map divByAbsGcd [(x,y) | x <- map (flip (-) x1) [minX..maxX], y <- map (flip (-) y1) [minY..maxY]]

    sortByOrigin :: Coord -> [Vec] -> [Vec]
    sortByOrigin o = sortOn (angle2P o)

    allCoords :: [Coord]
    allCoords = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]

    minX = 0
    minY = 0
    maxX = 41
    maxY = 41

    bestCoordAndCoverage :: WorldMap -> [Coord] -> (Coord, Int)
    bestCoordAndCoverage mappi = last . sortOn snd . map ((\cm -> (fst cm, length $ snd cm)) . (\c -> (c, mapScan mappi c)))
  
    sharedMain = do
        lns <- getInput "day10.txt"
        let lineLen = length lns
        let wm = evalState (populateMap $ concat lns) ([], 0)
        let mapRows = S.chunksOf lineLen $ S.fromList wm
        let mappi = foldl something M.empty (S.zip mapRows (S.fromList $ take (length mapRows) [0..]))
        return mappi

    part1 :: IO (Coord, Int)
    part1 = do
        mappi <- sharedMain
        return $ bestCoordAndCoverage mappi allCoords
    --
    data GameState = GS { hq :: Coord, vectors :: S.Seq Vec, cursor :: Int, vaporized :: [Coord], wmap :: WorldMap }
    vaporizerLoop :: State GameState [Coord]
    vaporizerLoop = do
        state <- get
        let maybeCurVec = S.lookup (cursor state) (vectors state)
        let maybeNextVec = S.lookup (cursor state + 1) (vectors state)
        let nextCursor = case maybeNextVec of
                            Nothing -> 0
                            Just _ -> succ $ cursor state
        if (==200) . length $ vaporized state
            then return $ vaporized state
            else case castRay (hq state) (wmap state) (fromJust maybeCurVec) of
              Void -> put (state { cursor = nextCursor }) >> vaporizerLoop
              Asteroid crd -> put (state { cursor = nextCursor, vaporized = crd : vaporized state, wmap = M.insert crd Void (wmap state) }) >> vaporizerLoop
    part2 = do
        mappi <- sharedMain
        let (best, _) = bestCoordAndCoverage mappi allCoords
        print best
        putStrLn "Lazors:"
        let lazors = S.fromList $ sortByOrigin (0,0) (allDirections best)
        print lazors
        putStrLn "Vaporized:"
        pure $ evalState vaporizerLoop GS { hq = best, vectors = lazors, cursor = 0, vaporized = [], wmap = mappi }
    -- part2: compile map
    -- for the asteroid that part1 returned the max for:
    -- sort Directions by angle2P and iterate through them, updating state map by replacing Asteroid with Void on each “first” hit
    -- loop through until 200 asteroids have been vaporized, then get that one’s coordinates.