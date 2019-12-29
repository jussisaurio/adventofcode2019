{-# LANGUAGE TupleSections #-}

module Day12 where
  import Parser

  data Pos = Pos { x :: Int, y :: Int, z :: Int }

  intParser :: Parser JSON
  intParser = Parser readInt where
            readInt :: String -> Either String (Int, String)
            readFloat str =
              let (numStr, xs) = span (\c -> isDigit c || c == '-') str in
                case (, xs) <$> readMaybe numStr of
                  Nothing -> Left ("Cannot parse primitive from " ++ str)
                  Just val -> Right val

  posParser :: Parser Pos
  posParser = char '<' *> 
  part1 = do
    file <- readFile "day12.txt"
