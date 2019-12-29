{-# LANGUAGE OverloadedStrings #-}

module Day9 where
  import Control.Monad.State
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import Data.Maybe (fromMaybe)
  import Data.List (permutations)
  import Text.Read (readMaybe)

  data Opcode = Add | Multiply | Input | Output | Halt | JumpT | JumpF | LThan | Equ | AdjustRB deriving (Eq, Show)
  data Parameter = Position Int | Immediate Int | Relative Int deriving (Eq, Show)
  data Instruction = I Opcode [Parameter] deriving (Eq, Show)
  data ProgramState = On | Paused | Halted deriving (Eq, Show)
  data MyState = MyState { program :: S.Seq Int, input :: [Int], output :: Int, pointer :: Int, rb :: Int, is :: ProgramState } deriving (Eq, Show)
  
  initS :: MyState
  initS = MyState { program = S.empty, input = [], output = 0, pointer = 0, rb = 0, is = On }

  getOpcode :: (Integral a, Eq a) => a -> Opcode
  getOpcode num
    | x == 1 = Add
    | x == 2 = Multiply
    | x == 3 = Input
    | x == 4 = Output
    | x == 5 = JumpT
    | x == 6 = JumpF
    | x == 7 = LThan
    | x == 8 = Equ
    | x == 9 = AdjustRB
    | x == 99 = Halt
    | otherwise = error "Invalid program"
    where x = num `mod` 100
  
  getNumArgs :: Opcode -> Int
  getNumArgs o 
    | o `elem` [Add, Multiply, LThan, Equ] = 3
    | o `elem` [JumpT, JumpF] = 2
    | o `elem` [Input, Output, AdjustRB] = 1
    | o == Halt = 0

  getParameter :: Int -> Int -> Int -> Parameter
  getParameter op paramNumber value
    | flag == 1 = Immediate value
    | flag == 2 = Relative value
    | otherwise = Position value
    where flag = (op `div` 10 ^ (paramNumber + 1)) `mod` 10

  safeGet :: MyState -> Int -> Int
  safeGet s x = fromMaybe 0 (S.lookup x (program s))

  pad :: Int -> S.Seq Int -> S.Seq Int
  pad dest p = case S.lookup dest p of
    Just _ -> p
    Nothing -> case S.lookup (dest - 1) p of
      Just _ -> S.insertAt dest 0 p
      Nothing -> pad dest $ pad (dest - 1) p

  upsert :: S.Seq Int -> Int -> Int -> S.Seq Int
  upsert p dest v = case S.lookup dest p of
    Just _ -> S.update dest v p
    Nothing -> S.insertAt dest v $ pad (dest - 1) p

  getParameterValue :: MyState -> Parameter -> Int
  getParameterValue s p =
    case p of
      Immediate x -> x
      Position x -> safeGet s x
      Relative x -> safeGet s (rb s + x)

  getInstruction :: S.Seq Int -> Int -> Instruction
  getInstruction program pointer = I opcode $ map (\paramNumber -> getParameter op paramNumber (offset paramNumber)) [1..numArgs]
    where
      offset = S.index program . (+) pointer
      op = offset 0
      opcode = getOpcode op
      numArgs = getNumArgs opcode

  exec :: MyState -> Instruction -> MyState
  exec state i =
    case i of
      I Halt _ -> state { is = Halted }
      I Add [p1,p2, dest] -> state { program = update dest (getValue p1 + getValue p2), pointer = oldIdx + 4 }
      I Multiply [p1,p2, dest]-> state { program = update dest (getValue p1 * getValue p2), pointer = oldIdx + 4 }
      I Input [dest] -> case input' of
        [] -> state { is = Paused }
        (x:xs) -> state { is = On, program = update dest x, pointer = oldIdx + 2, input = xs }
      I Output [src] -> state { output = getValue src, pointer = oldIdx + 2 }
      I JumpT [p1,p2] -> state { pointer = if getValue p1 == 0 then oldIdx + 3 else getValue p2 }
      I JumpF [p1,p2] -> state { pointer = if getValue p1 /= 0 then oldIdx + 3 else getValue p2 }
      I LThan [p1,p2, dest] -> state { program = update dest (if getValue p1 < getValue p2 then 1 else 0), pointer = oldIdx + 4 }
      I Equ [p1,p2, dest] -> state { program = update dest (if getValue p1 == getValue p2 then 1 else 0), pointer = oldIdx + 4 }
      I AdjustRB [p1] -> state { rb = rb state + getValue p1, pointer = oldIdx + 2 }
      where
        getValue = getParameterValue state
        input' = input state
        oldIdx = pointer state
        oldMyState = program state
        update dest = case dest of
          Position val -> upsert oldMyState val
          Relative val -> upsert oldMyState (rb state + val)

  loop :: MyState -> MyState
  loop state = let newState = exec state $ getInstruction (program state) (pointer state) in if is newState == On then loop newState else newState

  loopD :: MyState -> IO MyState
  loopD state = do
    let instruction = getInstruction (program state) (pointer state)
    -- print state
    print instruction
    let newState = exec state instruction in if is newState == On then loopD newState else pure newState


  readInt :: T.Text -> Int
  readInt = fromMaybe 0 . readMaybe . T.unpack

  splitByComma :: String -> [T.Text]
  splitByComma = T.splitOn "," . T.pack

  getInputAsInts :: String -> IO (S.Seq Int)
  getInputAsInts fn = do
    file <- readFile fn
    pure $ S.fromList $ map readInt $ splitByComma file

  getInput :: IO (S.Seq Int)
  getInput = getInputAsInts "day9.txt"
  
  -- run 0, get its output, put output to 1's input, run 1, get its output...
  type Amplifiers = S.Seq MyState
  feedbackLoop :: Int -> State Amplifiers Int
  feedbackLoop i = do
    states <- get
    let nextI = (i + 1) `mod` length states
    let lastAmplifier = S.index states (length states - 1) 
    let newS = loop $ S.index states i
    let newS2 = (S.index states nextI) { input = input (S.index states nextI) ++ [output newS] }
    if all ((== Halted) . is) states
      then return $ output lastAmplifier
      else let newStates = S.update nextI newS2 . S.update i newS $ states in put newStates >> feedbackLoop nextI
      
  go :: [Int] -> IO Int
  go sequence = do
    codes <- getInput
    let i = initS { program = codes }
    let is = [i { input = [0] }, i, i, i, i]
    pure $ maximum $ map (evalState (feedbackLoop 0) . S.fromList . zipWith (\s p -> s { input = p : input s }) is) $ permutations sequence

  boost inp debug = do
    codes <- getInput
    let i = initS { program = codes, input = [inp] }
    if debug then output <$> loopD i else pure . output $ loop i
  
  part1 = boost 1 True
  part2 = boost 2 False

  