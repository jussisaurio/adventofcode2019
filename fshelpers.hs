{-# LANGUAGE OverloadedStrings #-}

module FsHelpers where
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import Data.Maybe (fromMaybe)
  import Text.Read (readMaybe)

  readInt :: T.Text -> Int
  readInt = fromMaybe 0 . readMaybe . T.unpack

  splitByComma :: String -> [T.Text]
  splitByComma = T.splitOn "," . T.pack

  getInputAsInts :: String -> IO (S.Seq Integer)
  getInputAsInts fn = do
    file <- readFile fn
    pure $ S.fromList $ map (fromIntegral . readInt) $ splitByComma file