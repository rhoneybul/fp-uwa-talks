module Main where

import Data.List
import Data.List.Split
import System.IO
import Data.Char
import System.Environment

data Batsman = Batsman {
  name :: String,
  batting_average :: Float
} deriving Show

getRows :: String -> [[String]]
getRows t = do
  map (splitOn ", ") (lines t)

hasName :: String -> Batsman -> Bool
hasName s b = isInfixOf s ( map toLower (name b) )

getBatsman :: [String] -> Batsman
getBatsman x = do
  Batsman { name=n, batting_average=a } where
    n = x !! 0
    a = read (x !! 2) :: Float

main = do
  putStrLn "Search for a Batsman by Name: "
  searchTerm <- getLine
  contents <- readFile "batsmen.txt"
  let rows = getRows contents
  let batsmen = map getBatsman rows
  let results = filter (hasName searchTerm) batsmen
  if length results == 0
    then print "No Batsmen Found!"
    else print results
