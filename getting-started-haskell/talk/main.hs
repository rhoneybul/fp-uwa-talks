module Main where

import System.IO
import System.Environment
import Data.List
import Data.Char
import Data.List.Split

data Batsmen = Batsmen {
  name :: String,
  batting_average :: Float
} deriving (Show)

getRows :: String -> [[String]]
getRows x = map (splitOn ", ") (lines x)

getBatsmen :: [String] -> Batsmen
getBatsmen x = Batsmen { name = n, batting_average = a } where 
    n = x !! 0
    a = read (x !! 2) :: Float

hasName :: String -> Batsmen -> Bool
hasName s b = isInfixOf (map toLower s) ( map toLower (name b))

main = do
  putStrLn "Search for a Batsmen by Name: "
  searchTerm <- getLine
  contents <- readFile "batsmen.txt"
  let rows = getRows contents
  let batsmen = map getBatsmen rows
  let results = filter (hasName searchTerm) batsmen
  if length results == 0
    then print "No Batsmen Found"
    else print results
