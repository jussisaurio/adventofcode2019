{-# LANGUAGE OverloadedStrings #-}

module Day11 where
  import Intcode
  import qualified FsHelpers as Fsh
  import qualified Data.Map as M
  import Control.Monad.State
  import Data.Maybe (fromMaybe)
  import Data.List

  type Direction = Integer-- Up 0, Right 1, Down 2, Left 3
  type Coord = (Integer, Integer)

  black :: Integer
  black = 0
  white :: Integer
  white = 1
  leftTurn :: Integer
  leftTurn = 0
  rightTurn :: Integer
  rightTurn = 1

  type PanelPaintStatus = M.Map Coord Integer

  data GameState = GS { panels :: PanelPaintStatus, robotDir :: Direction, robotLoc :: Coord, icState :: IntcodeState }

  getPanelColor :: PanelPaintStatus -> Coord -> Integer
  getPanelColor pps c = fromMaybe 0 $ M.lookup c pps

  paintPanel :: PanelPaintStatus -> Coord -> Integer-> PanelPaintStatus
  paintPanel pps coord color = M.insert coord color pps

  turnRobot :: Direction -> Integer-> Direction
  turnRobot curDir turn
    | turn == leftTurn = if curDir == 0 then 3 else curDir - 1
    | turn == rightTurn = if curDir == 3 then 0 else curDir + 1
    | otherwise = error "invalid program"

  moveRobot :: Coord -> Direction -> Coord
  moveRobot (x,y) dir
    | dir == 0 = (x, y-1)
    | dir == 1 = (x+1,y)
    | dir == 2 = (x, y+1)
    | dir == 3 = (x-1, y)
    | otherwise = error "invalid program"


  paintLoop :: State GameState PanelPaintStatus
  paintLoop = do
    state <- get
    let ics = loop (icState state)
    case is ics of
      Halted -> return $ panels state
      Paused -> put (state { panels = newPanels, robotDir = newRobotDir, robotLoc = newRobotLoc, icState = ics { output = rest, input = [newInput] }}) >> paintLoop
                where
                  turn:color:rest = output ics -- outputs are prepended in the intcode computer so turn is "first"
                  newPanels = paintPanel (panels state) (robotLoc state) color
                  newRobotDir = turnRobot (robotDir state) turn
                  newRobotLoc = moveRobot (robotLoc state) newRobotDir
                  newInput = getPanelColor newPanels newRobotLoc
                  
  
  part1 = do
    codes <- Fsh.getInputAsInts "day11.txt"
    let i = initS { program = codes, input = [0] }
    let is = GS { panels = M.empty, robotDir = 0, robotLoc = (0,0), icState = i }
    pure . M.size $ evalState paintLoop is

  getRows ::  Coord -> [[Coord]] -> [[Coord]]
  getRows c acc = if null acc then [[c]] else let x:xs = acc in if snd c == snd (head x) then (c:x):xs else [c] : acc

  printColor :: Integer -> Char
  printColor 0 = ' '
  printColor 1 = '#'
  printColor _ = error "invalid program"

  part2 = do
    codes <- Fsh.getInputAsInts "day11.txt"
    let i = initS { program = codes, input = [1] }
    let im = M.insert (0,0) 1 M.empty
    let is = GS { panels = im, robotDir = 0, robotLoc = (0,0), icState = i }
    
    let finalPaintStatus = evalState paintLoop is
    let coordsSorted = sortOn snd $ M.keys finalPaintStatus
    let rows = foldr getRows [] coordsSorted
    let printables = map (map (printColor . getPanelColor finalPaintStatus)) rows
    print rows

  -- provide 0 (color of panel robot is on, 1 = white)
  -- robot will output paint color (0/1), turn l/r (0/1), and then it should always be moved forward
  -- then robot will probably be in "waiting for input" state, and we provide the color of the robots current location (0/1)
  -- loop until program halts 