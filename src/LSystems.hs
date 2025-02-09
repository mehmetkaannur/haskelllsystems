module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, trace1, trace2
                , expandLSystem, commandMap ) where

import IC.Colour

type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
data Command = F | L | R | B [Command]
  deriving Show
type ColouredLine = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem x _ _) = x
-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ x _) = x

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ x) = x

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar [] _ = []
lookupChar (x:xs) b
  | fst x == b = snd x
  | otherwise = lookupChar xs b

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne _ [] = []
expandOne x (y:ys) = lookupChar x y ++ expandOne x ys

--
-- Expand command string s n times using rule table r
--
expand :: [Char] -> Int -> Rules Char -> [Char]
expand x 0 _ = x
expand x a b = expand (expandOne b x) (a-1) b

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move R degree ((x, y), theta) = ((x, y), theta - degree)
move L degree ((x, y), theta) = ((x, y), theta + degree)
move F degree ((x, y), theta) = ((x+cos(rad), y + sin(rad)), theta)
  where
    rad = pi*(theta/180.0)

parse :: Rules Command -> [Char] -> [Command]
parse commandMap [] = []
parse commandMap (x:xs)
  | x == '[' = B (parse commandMap inbrac) : (parse commandMap outbrac)
  | otherwise = (lookupChar commandMap x) ++ (parse commandMap xs)
  where
    inbrac = takeWhile (']' /=) xs
    outbrac = dropWhile (']' /=) xs

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 xs a c = helper xs a c ((0.0,0.0), 90)
  where
    helper :: [Command] -> Float -> Colour -> TurtleState -> [ColouredLine]
    helper [] _ _ _ = []
    helper (F:xs) a c p@(v,f) = (v, t, c) : helper xs a c new
      where
        new@(t, _) = move F a p
    helper (R:xs) a c p@(v,f) = helper xs a c new
      where
        new = move R a p
    helper (L:xs) a c p@(v,f) = helper xs a c new
      where
        new = move L a p
    helper ((B cs):xs) a c p@(v,f) = helper cs a c p ++ helper xs a c p

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 xs a c = helper xs ((0.0,0.0), 90) [] a c
  where
    helper :: [Command] -> TurtleState -> [([Command], TurtleState)] -> Float -> Colour -> [ColouredLine]
    helper [] _ [] _ _ = []
    helper [] _ ((ys, t): qs) a c = helper ys t qs a c
    helper (F:xs) p@(v,f) s a c  = (v, t, c) : helper xs new s a c
      where
        new@(t, _) = move F a p
    helper (R:xs) p@(v,f) s a c  = helper xs new s a c
      where
        new = move R a p
    helper (L:xs) p@(v,f) s a c  = helper xs new s a c
      where
        new = move L a p
    helper ((B cs):xs) p@(v,f) s a c  = helper cs p ((xs, p):s) a c

-- Provided Functions
------------------------------------------------------------------------------

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem (LSystem _ axiom rs) n = parse commandMap (expand axiom n rs)

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]

