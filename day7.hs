{-# LANGUAGE OverloadedStrings #-}

module Day7 where
  import Control.Monad.State
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import Data.Maybe (fromMaybe)
  import Data.List (permutations)
  import Text.Read (readMaybe)

  data Opcode = Add | Multiply | Input | Output | Halt | JumpT | JumpF | LThan | Equ deriving (Eq, Show)
  data Parameter = Position Int | Immediate Int deriving (Eq, Show)
  data Instruction = I Opcode [Parameter] deriving (Eq, Show)
  data ProgramState = On | Paused | Halted deriving (Eq, Show)
  data MyState = MyState { program :: S.Seq Int, input :: [Int], output :: Int, pointer :: Int, is :: ProgramState } deriving (Eq, Show)
  
  initS :: MyState
  initS = MyState { program = S.empty, input = [], output = 0, pointer = 0, is = On }

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
    | x == 99 = Halt
    | otherwise = error "Invalid program"
    where x = num `mod` 100
  
  getNumArgs :: Opcode -> Int
  getNumArgs o 
    | o `elem` [Add, Multiply, LThan, Equ] = 3
    | o `elem` [JumpT, JumpF] = 2
    | o `elem` [Input, Output] = 1
    | o == Halt = 0

  getParameter :: Int -> Int -> Int -> Parameter
  getParameter op paramNumber value = if flag == 1 then Immediate value else Position value
    where flag = (op `div` 10^(paramNumber +1 )) `mod` 10

  getParameterValue :: MyState -> Parameter -> Int
  getParameterValue s p =
    case p of
      Immediate x -> x
      Position x -> S.index (program s) x

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
      I Add [p1,p2, Position dest] -> state { program = update dest (getValue p1 + getValue p2), pointer = oldIdx + 4 }
      I Multiply [p1,p2, Position dest]-> state { program = update dest (getValue p1 * getValue p2), pointer = oldIdx + 4 }
      I Input [Position dest] -> case input' of
        [] -> state { is = Paused }
        (x:xs) -> state { is = On, program = update dest x, pointer = oldIdx + 2, input = xs }
      I Output [src] -> state { output = getValue src, pointer = oldIdx + 2 }
      I JumpT [p1,p2] -> state { pointer = if getValue p1 == 0 then oldIdx + 3 else getValue p2 }
      I JumpF [p1,p2] -> state { pointer = if getValue p1 /= 0 then oldIdx + 3 else getValue p2 }
      I LThan [p1,p2, Position dest] -> state { program = update dest (if getValue p1 < getValue p2 then 1 else 0), pointer = oldIdx + 4 }
      I Equ [p1,p2, Position dest] -> state { program = update dest (if getValue p1 == getValue p2 then 1 else 0), pointer = oldIdx + 4 }
      where
        getValue = getParameterValue state
        input' = input state
        oldIdx = pointer state
        oldMyState = program state
        update dest val = S.update dest val oldMyState

  loop :: MyState -> MyState
  loop state = let newState = exec state $ getInstruction (program state) (pointer state) in if is newState == On then loop newState else newState

  readInt :: T.Text -> Int
  readInt = fromMaybe 0 . readMaybe . T.unpack

  splitByComma :: String -> [T.Text]
  splitByComma = T.splitOn "," . T.pack

  getInputAsInts :: String -> IO (S.Seq Int)
  getInputAsInts fn = do
    file <- readFile fn
    pure $ S.fromList $ map readInt $ splitByComma file

  getInput :: IO (S.Seq Int)
  getInput = getInputAsInts "day7.txt"
  
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
  
  part1 = go [0,1,2,3,4]
  part2 = go [5,6,7,8,9]

  