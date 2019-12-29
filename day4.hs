module Day4 where
  import Data.List (sort, group)
  powfloor :: Integer -> Integer
  powfloor num = floor(logBase 10 $ fromIntegral num)

  digit :: Integer -> Integer -> Integer
  digit which num = (num `div` (10^(powfloor num - which + 1))) `mod` 10

  neverDecrease :: Integer -> Bool
  neverDecrease num = a <= b && b <= c && c <= d && d <= e && e <= f
    where
      a = digit 1 num
      b = digit 2 num
      c = digit 3 num
      d = digit 4 num
      e = digit 5 num
      f = digit 6 num

  subtractFirst :: Integer -> Integer
  subtractFirst num = num - 10^floor(logBase 10 $ fromIntegral num) * digit 1 num

  -- largestDigit :: Integer -> Integer
  -- largestDigit num
  --     | num < 10 = num
  --     | otherwise = 

  ndec :: Integer -> Bool
  ndec num
      | num < 10 = True
      | otherwise = digit 1 num <= digit 2 num && ndec (subtractFirst num)


  adjacentSame :: Integer -> Bool
  adjacentSame num = a == b || b == c || c == d || d == e || e == f
    where
      a = digit 1 num
      b = digit 2 num
      c = digit 3 num
      d = digit 4 num
      e = digit 5 num
      f = digit 6 num

  adjacentSameOnlyTwo :: Integer -> Bool
  adjacentSameOnlyTwo num = (a == b && b /= c) || (b == c && a /= b && c /= d) || (c == d && b /= c && d /= e) || (d == e && c /= d && e /= f) ||  (e == f && d /= e)
    where
      a = digit 1 num
      b = digit 2 num
      c = digit 3 num
      d = digit 4 num
      e = digit 5 num
      f = digit 6 num

  satisfiesBoth :: Integer -> Bool
  satisfiesBoth num = ndec num && adjacentSame num

  satisfiesAll :: Integer -> Bool
  satisfiesAll num = ndec num && adjacentSameOnlyTwo num

  -- Someone's much more elegant version from reddit:
  
  digits 0 = []
  digits x = digits (x `div` 10) ++ [x `mod` 10]
  
  isNotDescending :: (Ord a, Eq a) => [a] -> Bool
  isNotDescending x = sort x == x
  hasDouble :: Eq a => [a] -> Bool
  hasDouble = any (>=2) . map length . group -- groups adjacent items that are equal, eg group [1,2,2,3,4,4,5] == [[1], [2,2], [3], [4,4], [5]]
  hasExactlyDouble :: Eq a => [a] -> Bool
  hasExactlyDouble = elem 2 . map length . group

  matching :: Int
  matching = length $ filter (\x -> isNotDescending x && hasExactlyDouble x) . map digits $ [138307..654504]