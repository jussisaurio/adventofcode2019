module Parser where
  import Control.Applicative
  import Data.Char (isDigit)

  newtype Parser t = Parser { runParser :: String -> Maybe (t, String) }

  satisfy :: (Char -> Bool) -> Parser Char
  satisfy predicate = Parser func
    where
      func [] = Nothing -- fail on empty input
      func (x:xs)
        | predicate x = Just (x, xs)
        | otherwise = Nothing

  char :: Char -> Parser Char
  char = satisfy . (==)

  first :: (a -> b) -> Parser a -> Parser b
  first mapper (Parser runParserInstance1) = Parser runParserInstance2
    where
      runParserInstance2 inputString = case runParserInstance1 inputString of
        Nothing -> Nothing
        Just (x, xs) -> Just (mapper x, xs)
  
  instance Functor Parser where
    fmap = first

  instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) (Parser f1) (Parser f2) = Parser f3
      where
        f3 inputString = f1 inputString <|> f2 inputString

  pureParser :: a -> Parser a
  pureParser val = Parser (\x -> Just (val, x))
  
  parserApply :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  parserApply mapper (Parser f1) (Parser f2) = Parser f3
    where
      f3 inputString = do
        (val1, rest1) <- f1 inputString
        (val2, rest2) <- f2 rest1
        pure (mapper val1 val2, rest2)
  
  instance Applicative Parser where
    pure = pureParser
    (<*>) = parserApply id

  zeroOrMore :: Parser a -> Parser [a]
  zeroOrMore p = oneOrMore p <|> pure []
  
  oneOrMore :: Parser a -> Parser [a]
  oneOrMore p = (:) <$> p <*> zeroOrMore p

  posIntParser :: Parser Integer
  posIntParser = Parser f
    where
      f inputString
        | null numberlikeString = Nothing
        | otherwise = Just (read numberlikeString, rest)
        where (numberlikeString, rest) = span isDigit inputString