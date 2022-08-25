{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c g = sum $ zipWith (\x y -> fromEnum (x == y)) c g

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
-- translate colors lst to occurences in input lst, lst
countColors :: Code -> [Int]
countColors xs = map (length . countColor) colors
  where countColor c = filter (==c) xs -- check input list for color

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code guess = sum $ zipWith min (countColors code) (countColors guess)

-- Exercise 3 -----------------------------------------
-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exmtch nonexmtch
  where exmtch = exactMatches secret guess
        mtch = matches secret guess
        nonexmtch = mtch - exmtch

-- Exercise 4 -----------------------------------------
getExactMatches :: Move -> Int
getExactMatches (Move _ em _) = em

getNonExactMatches :: Move -> Int
getNonExactMatches (Move _ _ nem) = nem

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exm nexm) code = (exm == exm') && (nexm == nexm')
  where movec = getMove guess code
        exm' = getExactMatches movec
        nexm' = getNonExactMatches movec

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------
allCodes :: Int -> [Code]
allCodes n
  | n <= 0 = []
  | otherwise = iterate (concatMap applyCode) basicColors !! (n-1)
      where basicColors = map (\x -> [x]) colors
            applyCode c = map (c++) basicColors
-- or helper recursive function that repeats f application

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = solve' secret (allCodes $ length secret)
  where
  solve' :: Code -> [Code] -> [Move]
  solve' sec codes
    | guessCode == sec = [guessMove]
    | otherwise = [guessMove] ++ solve' sec (filterCodes guessMove codes)
    where
      guessCode = head codes
      guessMove = getMove sec guessCode


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
