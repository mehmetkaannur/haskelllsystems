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
    rad = (pi*theta)/180.0

parse :: Rules Command -> [Char] -> [Command]
parse commandMap [] = []
parse commandMap (x:xs)
  | x == '[' = B (parse commandMap inbrac) : (parse commandMap outbrac)
  | otherwise = (lookupChar commandMap x) ++ (parse commandMap xs)
  where
    inbrac = takeWhile (']' /=) xs
    outbrac = dropWhile (']' /=) xs

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 (x:xs) a c = helper [x] 90 (0.0,0.0) : trace1 xs a c
  where
    helper :: [Command] -> Float -> TurtleState -> ColouredLine
    helper [F] b d = (fst d, fst (move F (b+a) d), c)
    helper [L] b d = (fst d, fst (move L (b-a) d), c)
    helper [R] b d = (fst d, fst (move R (b+a) d), c)
    helper [B (y:ys)] b d = helper [y] b d
    helper [] _ _  = []

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 = undefined

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

