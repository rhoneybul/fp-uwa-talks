{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Data.Aeson (ToJSON)
import System.IO.Unsafe
import System.IO 
import Data.List.Split
import GHC.Generics
import Data.Char 
import Data.List

data Batsmen = Batsmen {
    name :: String,
    average :: Float,
    runs :: Int,
    _id :: Int
} deriving (Show, Generic)

instance ToJSON Batsmen

getBatsmen :: [String] -> Batsmen
getBatsmen x = Batsmen { name = n, average = a, runs = r, _id = i } where 
    n = x !! 0
    r = read ( x !! 1 ) :: Int 
    a = read ( x !! 2 ) :: Float 
    i = read ( x !! 3 ) :: Int

getFileContents :: String 
getFileContents = unsafePerformIO (readFile "batsmen-data.txt")

getRows :: String -> [[String]]
getRows x = map (splitOn ", ") (lines x)

hasName :: String -> Batsmen -> Bool
hasName s b = isInfixOf (map toLower s) ( map toLower (name b))

batsmanSearch :: String -> [Batsmen] -> [Batsmen]
batsmanSearch s b = filter (hasName s) b

hasId :: Int -> Batsmen -> Bool 
hasId i b = (==) i (_id b)

idSearch :: Int -> [Batsmen] -> [Batsmen] 
idSearch i b = filter (hasId i) b  

allBatsmen :: [Batsmen]
allBatsmen = map getBatsmen (getRows getFileContents)

routes :: ScottyM ()
routes = do
    get "/" index 
    get "/search/:sstring" findByName
    get "/batsman/:id" findById
    get "/all/" returnAllBatsmen
    
returnAllBatsmen :: ActionM ()
returnAllBatsmen = json (allBatsmen)

findByName :: ActionM ()
findByName = do
    searchString <- param "sstring"
    let results = batsmanSearch searchString allBatsmen
    json $ results

findById :: ActionM ()
findById = do 
    uid <- param "id"
    let results = idSearch uid allBatsmen
    json $ results  

index :: ActionM ()
index = do
    html ("<h2>FP UWA Goes Web!</h2>")

main :: IO ()
main = do 
    putStrLn "FP UWA Web Service Started on PORT: 5000"
    scotty 5000 routes 
