{-# LANGUAGE OverloadedStrings #-}

module Intcode where
  import Control.Monad.State
  import qualified Data.Text as T
  import qualified Data.Sequence as S
  import Data.Maybe (fromMaybe)
  import Data.List (permutations)
  import Text.Read (readMaybe)

  data Opcode = Add | Multiply | Input | Output | Halt | JumpT | JumpF | LThan | Equ | AdjustRB deriving (Eq, Show)
  data Parameter = Position Integer | Immediate Integer | Relative Integer deriving (Eq, Show)
  data Instruction = I Opcode [Parameter] deriving (Eq, Show)
  data ProgramState = On | Paused | Halted deriving (Eq, Show)
  data IntcodeState = IntcodeState { program :: S.Seq Integer, input :: [Integer], output :: [Integer], pointer :: Integer, rb :: Integer, is :: ProgramState } deriving (Eq, Show)
  
  initS :: IntcodeState
  initS = IntcodeState { program = S.empty, input = [], output = [], pointer = 0, rb = 0, is = On }

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
  
  getNumArgs :: Opcode -> Integer
  getNumArgs o 
    | o `elem` [Add, Multiply, LThan, Equ] = 3
    | o `elem` [JumpT, JumpF] = 2
    | o `elem` [Input, Output, AdjustRB] = 1
    | o == Halt = 0

  getParameter :: Integer -> Integer -> Integer -> Parameter
  getParameter op paramNumber value
    | flag == 1 = Immediate value
    | flag == 2 = Relative value
    | otherwise = Position value
    where flag = (op `div` 10 ^ (paramNumber + 1)) `mod` 10

  safeGet :: IntcodeState -> Integer -> Integer
  safeGet s x = fromMaybe 0 (S.lookup (fromIntegral x) (program s))

  pad :: Integer -> S.Seq Integer -> S.Seq Integer
  pad dest p = case S.lookup (fromIntegral dest) p of
    Just _ -> p
    Nothing -> case S.lookup (fromIntegral (dest - 1)) p of
      Just _ -> S.insertAt (fromIntegral dest) 0 p
      Nothing -> pad dest $ pad (dest - 1) p

  upsert :: S.Seq Integer -> Integer -> Integer -> S.Seq Integer
  upsert p dest v = case S.lookup (fromIntegral dest) p of
    Just _ -> S.update (fromIntegral dest) v p
    Nothing -> S.insertAt (fromIntegral dest) v $ pad (dest - 1) p

  getParameterValue :: IntcodeState -> Parameter -> Integer
  getParameterValue s p =
    case p of
      Immediate x -> x
      Position x -> safeGet s x
      Relative x -> safeGet s (rb s + x)

  getInstruction :: S.Seq Integer -> Integer -> Instruction
  getInstruction program pointer = I opcode $ map (\paramNumber -> getParameter op paramNumber (offset paramNumber)) [1..numArgs]
    where
      offset a = S.index program $ (+) (fromIntegral pointer) (fromIntegral a)
      op = offset 0
      opcode = getOpcode op
      numArgs = getNumArgs opcode

  exec :: IntcodeState -> Instruction -> IntcodeState
  exec state i =
    case i of
      I Halt _ -> state { is = Halted }
      I Add [p1,p2, dest] -> state { program = update dest (getValue p1 + getValue p2), pointer = oldIdx + 4 }
      I Multiply [p1,p2, dest]-> state { program = update dest (getValue p1 * getValue p2), pointer = oldIdx + 4 }
      I Input [dest] -> case input' of
        [] -> state { is = Paused }
        (x:xs) -> state { is = On, program = update dest x, pointer = oldIdx + 2, input = xs }
      I Output [src] -> state { output = getValue src : output state, pointer = oldIdx + 2 }
      I JumpT [p1,p2] -> state { pointer = if getValue p1 == 0 then oldIdx + 3 else getValue p2 }
      I JumpF [p1,p2] -> state { pointer = if getValue p1 /= 0 then oldIdx + 3 else getValue p2 }
      I LThan [p1,p2, dest] -> state { program = update dest (if getValue p1 < getValue p2 then 1 else 0), pointer = oldIdx + 4 }
      I Equ [p1,p2, dest] -> state { program = update dest (if getValue p1 == getValue p2 then 1 else 0), pointer = oldIdx + 4 }
      I AdjustRB [p1] -> state { rb = rb state + getValue p1, pointer = oldIdx + 2 }
      where
        getValue = getParameterValue state
        input' = input state
        oldIdx = pointer state
        oldIntcodeState = program state
        update dest = case dest of
          Position val -> upsert oldIntcodeState val
          Relative val -> upsert oldIntcodeState (rb state + val)

  loop :: IntcodeState -> IntcodeState
  loop state = let newState = exec state $ getInstruction (program state) (pointer state) in if is newState == On then loop newState else newState

  loopD :: IntcodeState -> IO IntcodeState
  loopD state = do
    let instruction = getInstruction (program state) (pointer state)
    -- print state
    print instruction
    let newState = exec state instruction in if is newState == On then loopD newState else pure newState