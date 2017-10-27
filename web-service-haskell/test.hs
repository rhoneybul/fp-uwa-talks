{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Web.Scotty

-- define our routes 
routes :: ScottyM ()
routes = do 
  get "/" index 

index :: ActionM ()
index = html ("<h2>FP UWA Goes Web!</h2>")

main :: IO ()
main = do
  putStrLn "FP UWA Web Service Started on PORT: 5000"
  scotty 5000 routes