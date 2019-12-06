{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import PreludeAoC
import qualified RIO.List as List
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified Text.Megaparsec.Char.Lexer as L

{-
--- Day 3: Crossed Wires ---

The gravity assist was successful, and you're well on your way to the Venus
refuelling station. During the rush back on Earth, the fuel management system
wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are
connected to a central port and extend outward on a grid. You trace the path
each wire takes as it leaves the central port, one wire per line of text (your
puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the
circuit, you need to find the intersection point closest to the central port.
Because the wires are on a grid, use the Manhattan distance for this
measurement. While the wires do technically cross right at the central port
where they both start, this point does not count, nor does a wire count as
crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the
central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........

Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

These wires cross at two locations (marked X), but the lower-left one is closer
to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

    R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72
    U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83 = distance 159
    R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20, R 33,U 53,R 51
    U 98,R 91,D 20,R 16,D 67,R 40,U 7, R 15, U 6,R 7 = distance 135

What is the Manhattan distance from the central port to the closest intersection?

--- Part Two ---

It turns out that this circuit is very timing-sensitive; you actually need to
minimize the signal delay.

To do this, calculate the number of steps each wire takes to reach each
intersection; choose the intersection where the sum of both wires' steps is
lowest. If a wire visits a position on the grid multiple times, use the steps
value from the first time it visits that position when calculating the total
value of a specific intersection.

The number of steps a wire takes is the total number of grid squares the wire has entered to get to that location, including the intersection being considered. Again consider the example from above:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

In the above example, the intersection closest to the central port is reached
after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the second
wire for a total of 20+20 = 40 steps.

However, the top-right intersection is better: the first wire takes only 8+5+2 =
15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.

Here are the best steps for the extra examples from above:

    R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
    R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps

What is the fewest combined steps the wires must take to reach an intersection?


-}

data Direction = R Int | L Int | U Int | D Int
  deriving (Eq, Ord, Show)

parseDirection :: Parser Direction
parseDirection = pDir <*> L.decimal

pDir :: Parser (Int -> Direction)
pDir = single 'R' *> pure R
  <|> single 'L' *> pure L
  <|> single 'U' *> pure U
  <|> single 'D' *> pure D

parseDirections :: Parser ([Direction], [Direction])
parseDirections = (,) <$> pDirs <* newline <*> pDirs
  where
    pDirs = parseDirection `sepBy` single ','

type PositionSteps = Map (Int, Int) Int

positions :: [Direction] -> PositionSteps
positions dirs = Map.delete (0,0) $ go (0,0) 0 dirs mempty
  where
    go _ _ [] ps = ps
    go (x,y) s (d:ds) ps = case d of
      R n -> go (x+n,y) (s+n) ds $ foldr (\i -> Map.insertWith min (x+i,y) (s+i)) ps [1..n]
      L n -> go (x-n,y) (s+n) ds $ foldr (\i -> Map.insertWith min (x-i,y) (s+i)) ps [1..n]
      U n -> go (x,y+n) (s+n) ds $ foldr (\i -> Map.insertWith min (x,y+i) (s+i)) ps [1..n]
      D n -> go (x,y-n) (s+n) ds $ foldr (\i -> Map.insertWith min (x,y-i) (s+i)) ps [1..n]

minManhattanDistanceToOrigin :: [Direction] -> [Direction] -> Maybe Int
minManhattanDistanceToOrigin d1 d2 =
  let dist (x1,y1) = abs x1 + abs y1
  in fmap fst $ Set.minView $ Set.map dist $ intersections d1 d2

minSumOfWireLengthWithIntersection :: [Direction] -> [Direction] -> Maybe Int
minSumOfWireLengthWithIntersection d1 d2 =
  let p1 = positions d1
      p2 = positions d2
  in listToMaybe $ List.sort $ Map.elems $ Map.intersectionWith (+) p1 p2


intersections :: [Direction] -> [Direction] -> Set (Int, Int)
intersections d1 d2 =
  let p1 = Map.keysSet $ positions d1
      p2 = Map.keysSet $ positions d2
  in p1 `Set.intersection` p2

solution1 :: IO (Maybe Int)
solution1 =
  parseFile "input/Day3-1.txt" parseDirections
    <&> uncurry minManhattanDistanceToOrigin

solution2 :: IO (Maybe Int)
solution2 =
  parseFile "input/Day3-1.txt" parseDirections
    <&> uncurry minSumOfWireLengthWithIntersection
