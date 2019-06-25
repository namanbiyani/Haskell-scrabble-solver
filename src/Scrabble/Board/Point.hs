
-- Simple code to represent (x,y) coordinates on a plane
-- where the coordinate (0,0) is in the upper left hand corner
-- for example in this grid:
--
--   a,b,c
--   d,e,f
--   g,h,i
--
-- a is at position (0,0), and i is at position (2,2)
module Scrabble.Board.Point (
   Point
 -- * functions that return a single point
 , aboveP
 , belowP
 , leftOfP
 , rightOfP
 -- ** functions that return lists of points
 , allAboveP
 , allBelowP
 , allRightOfP
 , allLeftOfP
 , neighbors4P
) where

type Point = (Int, Int)

-- return the point directly above the given point
aboveP :: Point -> Point
aboveP   (x, y) = (x, y - 1)

-- | return the point directly below the given point
belowP :: Point -> Point
belowP   (x, y) = (x, y + 1)

-- return the point directly above the given point
leftOfP :: Point -> Point
leftOfP  (x, y) = (x - 1, y)

-- return the point directly above the given point
rightOfP :: Point -> Point
rightOfP (x, y) = (x + 1, y)


-- returns all points above p 
allAboveP :: Point -> [Point]
allAboveP (x, y) = zip (repeat x) [y - 1, y - 2 ..0]

-- returns all points below p
allBelowP :: Point -> [Point]
allBelowP (x, y) = zip (repeat x) [y + 1, y + 2 ..12]

-- returns all points to the right of p
allRightOfP :: Point -> [Point]
allRightOfP (x, y) = zip [x + 1, x + 2 ..12] (repeat y)

-- returns all points to the left of p
allLeftOfP :: Point -> [Point]
allLeftOfP (x, y) = zip [x - 1, x - 2 .. ] (repeat y)

-- returns all neighbours of p
neighbors4P :: Point -> [Point]
neighbors4P p = [aboveP p, belowP p, leftOfP p, rightOfP p]