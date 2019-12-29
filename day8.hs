module Day8 where
  import Data.List (minimumBy, foldl')
  import Data.List.Split (chunksOf)

  countOf :: Eq a => a -> [a] -> Int
  countOf el = length . filter (== el)

  leastZeros [] = []
  leastZeros strs = minimumBy (\a b -> if countOf '0' a > countOf '0' b then GT else LT) strs

  part1 :: IO ()
  part1 = do
    layers <- chunksOf 150 <$> readFile "day8.txt"
    let foo = leastZeros layers in print $ countOf '1' foo * countOf '2' foo
  
  layerPrecedence :: Char -> Char -> Char
  layerPrecedence '2' c = c
  layerPrecedence c _ = c

  easierToReadInStdout :: Char -> Char
  easierToReadInStdout '1' = 'X'
  easierToReadInStdout '0' = ' '
  easierToReadInStdout _ = error "invalid program"

  width = 25
  height = 6
  layerSize = width * height

  part2 :: IO ()
  part2 = chunksOf width . map easierToReadInStdout . foldl' (zipWith layerPrecedence) (replicate layerSize '2') . chunksOf layerSize <$> readFile "day8.txt" >>= mapM_ print