{-# LANGUAGE OverloadedStrings #-}

module Day5 where
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import Data.Maybe (fromMaybe)
  import Text.Read (readMaybe)

  data Opcode = Add | Multiply | Input | Output | Halt | JumpT | JumpF | LThan | Equ deriving (Eq, Show)
  data Parameter = Position Int | Immediate Int deriving (Eq, Show)
  data Instruction = I Opcode [Parameter] deriving (Eq, Show)
  data State = State { program :: S.Seq Int, input :: Int, output :: Int, idx :: Int } deriving (Eq, Show)
  initialState :: State
  initialState = State { program = S.empty, input = 0, output = 0, idx = 0 }

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
    | otherwise = error "Invalid program"

  getParameter :: Int -> Int -> Int -> Parameter
  getParameter op paramNumber value = if flag == 1 then Immediate value else Position value
    where flag = (op `div` 10^(paramNumber +1 )) `mod` 10

  getParameterValue :: State -> Parameter -> Int
  getParameterValue s p =
    case p of
      Immediate x -> x
      Position x -> S.index (program s) x

  getInstruction :: S.Seq Int -> Int -> Instruction
  getInstruction program index = I opcode $ map (\paramNumber -> getParameter op paramNumber (offset paramNumber)) [1..numArgs]
    where
      offset = S.index program . (+) index
      op = offset 0
      opcode = getOpcode op
      numArgs = getNumArgs opcode

  exec :: State -> Instruction -> State
  exec state i =
    case i of
      I Add [p1,p2, Position dest] -> state { program = update dest (getValue p1 + getValue p2), idx = oldIdx + 4 }
      I Multiply [p1,p2, Position dest]-> state { program = update dest (getValue p1 * getValue p2), idx = oldIdx + 4 }
      I Input [Position dest] -> state { program = update dest input', idx = oldIdx + 2 }
      I Output [src] -> state { output = getValue src, idx = oldIdx + 2 }
      I JumpT [p1,p2] -> state { idx = if getValue p1 == 0 then oldIdx + 3 else getValue p2 }
      I JumpF [p1,p2] -> state { idx = if getValue p1 /= 0 then oldIdx + 3 else getValue p2 }
      I LThan [p1,p2, Position dest] -> state { program = update dest (if getValue p1 < getValue p2 then 1 else 0), idx = oldIdx + 4 }
      I Equ [p1,p2, Position dest] -> state { program = update dest (if getValue p1 == getValue p2 then 1 else 0), idx = oldIdx + 4 }
      where
        getValue = getParameterValue state
        input' = input state
        oldIdx = idx state
        oldState = program state
        update dest val = S.update dest val oldState

  loop :: State -> IO Int
  loop state = do
        let index = idx state
        let prg = program state
        let instruction = getInstruction prg index
        print instruction
        case instruction of
          I Halt _ -> pure $ output state
          _ -> loop $ exec state instruction

  readInt :: T.Text -> Int
  readInt = fromMaybe 0 . readMaybe . T.unpack

  splitByComma :: String -> [T.Text]
  splitByComma = T.splitOn "," . T.pack

  getInputAsInts :: String -> IO (S.Seq Int)
  getInputAsInts fn = do
    file <- readFile fn
    pure $ S.fromList $ map readInt $ splitByComma file

  go :: Int -> IO ()
  go i = getInputAsInts "day5.txt" >>= \codes -> loop initialState { program = codes, input = i } >>= print

  part1 :: IO ()
  part1 = go 1

  part2 :: IO ()
  part2 = go 5

  main = part2

  