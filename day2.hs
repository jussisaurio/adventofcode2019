{-# LANGUAGE OverloadedStrings #-}

module Day2 where
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import Data.Maybe (fromMaybe)
  import Text.Read (readMaybe)

  readInteger :: T.Text -> Int
  readInteger = fromMaybe 0 . readMaybe . T.unpack

  splitByComma :: String -> [T.Text]
  splitByComma = T.splitOn "," . T.pack

  getInputAsIntegers :: String -> IO (S.Seq Int)
  getInputAsIntegers fn = do
    file <- readFile fn
    pure $ S.fromList $ map readInteger $ splitByComma file

  data Opcode = Add | Multiply | Halt | Unrecognized deriving (Eq, Show)
  getOpcode :: (Num a, Eq a) => a -> Opcode
  getOpcode x
    | x == 1 = Add
    | x == 2 = Multiply
    | x == 99 = Halt
    | otherwise = Unrecognized

  programLoop1 :: S.Seq Int -> Int -> Int
  programLoop1 codes idx =
    case getOpcode $ S.index codes idx of
      Halt -> S.index codes 0
      Unrecognized -> -1
      Add -> programLoop1 (S.update outputPos newValue codes) (idx + 4)
        where
          newValue = firstInput + secondInput
      Multiply -> programLoop1 (S.update outputPos newValue codes) (idx + 4)
        where
          newValue = firstInput * secondInput
      where
        outputPos = S.index codes (idx + 3)
        firstInput = S.index codes (S.index codes (idx+1))
        secondInput = S.index codes (S.index codes (idx+2))

  part1 :: IO Int
  part1 = do
    codes <- S.update 2 2 . S.update 1 12 <$> getInputAsIntegers "day2.txt"
    pure $ programLoop1 codes 0
  
  programLoop2 :: S.Seq Int -> Int -> Int -> [Int]
  programLoop2 codes noun verb
    | result == 19690720 = [noun, verb]
    | otherwise =
        if noun == 99 then programLoop2 codes 0 (verb + 1)
        else programLoop2 codes (noun+1) verb
    where result = programLoop1 (S.update 2 verb . S.update 1 noun $ codes) 0

  part2 = do
    originalCodes <- getInputAsIntegers "day2.txt"
    pure $ programLoop2 originalCodes 0 0

  main :: IO [Int]
  main = part2

  
