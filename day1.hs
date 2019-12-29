module Day1 where

  import Data.Maybe (fromMaybe)
  import Data.Bool (bool)
  import Text.Read (readMaybe)
  import Control.Monad

  getInteger :: String -> Integer
  getInteger = fromMaybe 0 . readMaybe
  
  -- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c 
  -- where m = IO, a = String, b = String, c = [Integer]
  getLinesAsInts :: String -> IO [Integer]
  getLinesAsInts = pure . map getInteger . lines <=< readFile

  -- fuelRequired :: Integer -> Integer
  fuelRequired = pure bool <*> const 0 <*> flip (-) 2 . floor . (/3) . fromIntegral <*> (>8)

  fuelRequiredTakeFuelMassIntoAccount :: Integer -> Integer
  fuelRequiredTakeFuelMassIntoAccount mass
   | result > 0 = result + fuelRequiredTakeFuelMassIntoAccount result
   | otherwise = 0
   where result = fuelRequired mass

  part1 :: IO Integer
  part1 = sum . map fuelRequired <$> getLinesAsInts "day1.txt"

  part2 :: IO Integer
  part2 = sum . map fuelRequiredTakeFuelMassIntoAccount <$> getLinesAsInts "day1.txt"

  main :: IO Integer
  main = part2
