module Robot
where

import Types
import Data.List
import Data.Array

-- data RobotMem mem = Empty | Point mem
data RobotMem = RobotMem {
   currentPos :: Point -- robot's current position
   , oldSensorVal :: SVal -- previous sensor val 
   , oldPos :: [Point] -- previous positions
} deriving (Show)
{-
When the robot enters the mine it receives as input the size of the mine (it
is always placed at (0, 0)). This function should return the initial memory
element of the robot.
-}
startRobot :: Size -> RobotMem
startRobot size = RobotMem {currentPos = (0, 0), oldSensorVal = 0, oldPos = [(0, 0)]}
{-
At each time step the robot sends a light beam in all 4 cardinal directions,
receives the reflected rays and computes their intensity (the first argument
of the function).

The robot sees nearby pits. The second argument of this function is the list
of neighbouring pits near the robot (if empty, there are no pits).

Taking into account the memory of the robot (third argument of the function),
it must return a tuple containing a new cardinal direction to go to and a new
memory element.

If the cardinal direction chosen goes to a pit or an wall the robot is
destroyed. If the new cell contains minerals they are immediately collected.
-}
increase :: Point -> Cardinal -> Point
increase (x, y) N = (x - 1, y)
increase (x, y) S = (x + 1, y)
increase (x, y) E = (x, y + 1)
increase (x, y) W = (x, y - 1)

direction :: [Cardinal] -> SVal -> [Cardinal] -> RobotMem -> Cardinal
direction (hd:tl) s cs m
   | elem hd cs = direction tl s cs m
   | elem (increase (currentPos m) hd) (oldPos m) = direction tl s cs m
   | otherwise = hd
--    | otherwise  = oldPos m ++ [increase (currentPos m) hd]
--       return hd
--    | elem N cs = S
--    | elem W cs = E
--    | elem S cs = N
--    | elem E cs = W
--    | otherwise = S

perceiveAndAct :: SVal -> [Cardinal] -> RobotMem -> (Action, RobotMem)
-- perceiveAndAct s cs m = (Nothing, m) -- TODO

-- x=getDirection(maze memory)
-- s = sensor value
-- cs = blocked cells
-- m = robot's memory
perceiveAndAct s cs m = do 
   let dir = direction [E, S, N, W] s cs m
   let newpos = increase (currentPos m) dir
   let mem = RobotMem {currentPos = newpos, oldSensorVal = 0, oldPos = oldPos m ++ [newpos]}
   (Just dir, mem)