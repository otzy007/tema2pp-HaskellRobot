module Robot
where

import Types
import Data.List
import Data.Array

data RobotMem = RobotMem {
   currentPos :: Point -- robot's current position
   , oldSensorVal :: SVal -- previous sensor val 
   , oldPos :: [Point] -- previous positions
}
{-
When the robot enters the mine it receives as input the size of the mine (it
is always placed at (0, 0)). This function should return the initial memory
element of the robot.
-}
startRobot :: Size -> RobotMem
startRobot size = RobotMem {
   currentPos = (0, 0)
   , oldSensorVal = 0
   , oldPos = [(0, 0)]
}

-- used to set new directions coordinates
increase :: Point -> Cardinal -> Point
increase (x, y) N = (x - 1, y)
increase (x, y) S = (x + 1, y)
increase (x, y) E = (x, y + 1)
increase (x, y) W = (x, y - 1)

-- Set the direction to go except blocked and already visited ones
direction :: [Cardinal] -> SVal -> [Cardinal] -> RobotMem -> Cardinal
direction possibleDir s cs m
   -- if there are no possible directions empty the memory and try again 
   | possibleDir == [] = direction [E, S, N, W] s cs RobotMem {
      currentPos  = currentPos m			 
      , oldSensorVal = oldSensorVal m
      , oldPos = drop ((length (oldPos m)) - 2) (oldPos m) 
	 ++ [currentPos m]
   } 
   -- exclude the blocked directions
   | elem (head possibleDir) cs = direction (tail possibleDir) s cs m
   -- if the sensor value is smaller than the old one the robot can go back
   | s < oldSensorVal m = direction possibleDir s cs RobotMem {
      currentPos  = currentPos m			 
      , oldSensorVal = s
      , oldPos = (reverse (tail (reverse (oldPos m)))) 
   } 
   --exclude the already visited directions
   | elem (increase (currentPos m) (head possibleDir)) (oldPos m) = 
      direction (tail possibleDir) s cs m 
   | otherwise = head possibleDir --return the direction

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
perceiveAndAct :: SVal -> [Cardinal] -> RobotMem -> (Action, RobotMem)
perceiveAndAct s cs m = do 
   let dir = direction [E, N, S, W] s cs m -- set the new direction
   let newpos = increase (currentPos m) dir -- set the new position
   let mem = RobotMem {
      currentPos = newpos
      , oldSensorVal = s		-- update the memory
      , oldPos = oldPos m ++ [newpos]
   }
   (Just dir, mem) --issue order