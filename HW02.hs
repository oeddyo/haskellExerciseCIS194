{-# OPTIONS_GHC -Wall #-}

module HW02 where
import Debug.Trace

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys) = if x==y then (1 + rest) else rest where rest = exactMatches xs ys

-- Exercise 2 -----------------------------------------

countOneColor :: Code -> Peg -> Int
countOneColor cs p = length $ (filter (==p) cs)

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors curCode = map (countOneColor curCode) colors


tupleMin :: (Int, Int) -> Int
tupleMin (a, b) = min a b
-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ map tupleMin (zip (countColors a) (countColors b))

-- Exercise 3 -----------------------------------------

nonExactMatches :: Code -> Code -> Int
nonExactMatches secret guess = (matches secret guess) - (exactMatches secret guess)

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess nExactMatches nNonExactMatches
    where nExactMatches = exactMatches secret guess
          nNonExactMatches = nonExactMatches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move codeInMove _ _) guess = if nExact == nNoneExact then True else False 
    where nExact = exactMatches codeInMove guess 
          nNoneExact = nonExactMatches codeInMove guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move listOfCodes = filter (isConsistent move) listOfCodes 

-- Exercise 6 -----------------------------------------

debug = flip trace


allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concat $ map (\x -> map (\y -> x: y) (allCodes(n-1)))  colors  



-- Exercise 7 -----------------------------------------

solveHelper :: Code -> [Move] -> [Move]
solveHelper _ [] = []
--solveHelper secret [elem] = [elem]
solveHelper secret moves = if length moves == length afterFilter then moves else solveHelper secret (filter (\x -> isConsistent x secret) moves)
                        where afterFilter = filter (\x -> isConsistent x secret) moves

solve :: Code -> [Move]
solve secret = solveHelper secret (map (\x -> Move x (exactMatches secret x) (nonExactMatches secret x)) (allCodes 4))
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined