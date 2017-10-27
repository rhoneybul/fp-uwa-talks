![](https://www.fpcomplete.com/hubfs/haskell_logo.svg?t=1507830794816 =100x)

### Building a Web Service in Haskell

Continuing on from last week, where we had a gentle introduction to using ghc and Haskell, we are going to build a simple json web service using the haskell module 'Scotty'.

### Scotty 

Scott is a Haskell Web Framework, which is very lightweight and simplistic to use. To install scotty on your machine, run the following command in the terminal;


```{bash}
honeybul@ubuntu:~$ cabal install scotty
```

### Today's Exercise

Im am going to give a gentle introduction to using Scotty by building a very simple JSON web service API, which uses the same batsmen data that we worked with last week. We will set this up so we could build a very simple frontend web service to access this data. We will set up a service that will return a json object of batsmen whose name matches a particular search string, and a route where we can return a batsmen given their unique id.

```{haskell}
['GET'] ('/search/:sstring')
  -- Returns a json object, which gives a list of batsmen whose name matches the ':sstring'

['GET'] ('/batsman/:id') => 
  -- Returns the batsman with that particular id
```

### Setting up the Web Service

As we did last week, we will set up a 'main.hs' file, which contains the logic for the web service. 

```{haskell}
module Main where

main = do 
    putStrLn "Hello, World"

```

Now, we need to import scotty, which is the Haskell Web Framework we shall be using for building the web service. We will also need to use 'Language Overloaded Strings', which allows use strings for things like routes. So the top of our file will look like this;

```{haskell}
{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Web.Scotty
```

#### Basic Scotty Functions

There are a few basic scotty functions we will be using today. 

* scotty - takes two parameters, an integer and routes (we will get into this in a second). This starts a web service on the port specified, with the routes specified.
* html - takes one parameter, a html string, which then is rendered when the functin is called
* json - takes one parameter, which is data, or a list of data, which is rendered as JSON when the function is called


#### Scotty Types / Monads

Scotty has a few basic monads, which are responsible for creating the routes and views. 

* The ScottyM monad defines the routes. We define all our routes within a 'do' statement. each route is given by the type of request (get/post/ etc.), the route address, and the function we wish to call when a user hits that route. 
* The ActionM monad defines a view, and is the type of function which is called by a route in the ScottyM monad. A funciton of type ActionM should return either some text, html or json. 


Let's get started!

let's create a very simple index route, which will serve up an 'FP UWA Goes Web!' message. Let's set up a function to handle the routes, which is a function that produces an ActionM monad, and on route "/", 'gets' the index function. In Scotty, get is a function which takes the address for which to serve the route, and the function to call when we render that route. 

```{haskell}
routes :: ScottyM ()
routes = do
  get "/" index
```

So, we need to create a function 'index', that renders some html, saying 'FP UWA Goes Web'. As I mentioned, a 'view' is a function of type 'ActionM', so we can create the index function as follows;

```{haskell}
index :: ActionM()
index = html ("<h2>FP UWA Goes Web!</h2>")
```

Cool, so we have set up our route, and our view. So, as always, we need a main function, which we want to call the scotty function, with a particular port to serve the web service on and the routes which we have defined. Let's also print a message saying that the server is started, just to be thorough. 

```{haskell}
main :: IO ()
main = do
  putStrLn "FP UWA Web Service Started on PORT: 5000"
  scotty 5000 routes
```

Now, if we load in our module within ghci, and run, we should start our web service on PORT 5000!

```{bash}
honeybul@ubuntu:~$ ghci
Prelude> :l main
[1 of 1] Compiling Main             ( main.hs, interpreted )
Ok, modules loaded: Main.
*Main> main
FP UWA Web Service Started on PORT: 5000
```

Now, if we go to http://localhost:5000/, we should see our lovely web service! 

## Today's Challenge

Con

```{haskell}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

data Incident = Incident {
  year :: Int, 
  incidentType :: String, 
  location :: String
} deriving (Show, Generic)

```

Once again, we use show and generic in this case, so that we can print if need be. 

Firstly, let's read in the file, with unsafePerformIO (which to use we need to import System.IO.Unsafe, and System.IO). This function getFileContents, gets a string, with the entire file contents.

```{haskell}
import System.IO.Unsafe 
import System.IO

getFileContents :: String
getFileContents = unsafePerformIO (readFile "terrorist-incidents.tsv")
```
Now, we have the entire file, as a string. Let's convert this string, to a list of incidents, with a function getRows, which will convert our string, to a list of rows, where each row is a list of strings, where the first string gives the incdient year, the second gives the incident type, and the third gives the location. So this function needs to split on end of line character, then for every element in this list, we need to split that on a tab character. Since we are splitting on a charcter, we need to import Data.List.Split

```{haskell}
import Data.List.Split

getRows :: String -> [[String]]
getRows x = do
  map (splitOn "\t") (lines x)
```

Since getRows takes a string, and getFileContents, is a string (since it produces a string, EVERYTHING IS A FUNCTION IN HASKELL), we can get the file, as a list of lists, by;

```{haskell}
let contents = getRows getFileContents 
```

Now, let's make a function that takes a row, and produces an incident. Remember, to convert a string into an integer, we 'read' the string into an Int.

```{haskell}

getIncident :: [String] -> Incident 
getIncident x = Incident {year = yr, incidentType = it, location=lo} where 
  year = read ( x !! 0 ) :: Int 
  it = x !! 1
  lo = x !! 2
```

So, to get our Incidents, we can map this function, over our contents!

```{haskell}
getIncidents :: [[String]] -> [Incident]
getIncidents x = map getIncident x 
```

So, to simplify things, lets make a get data function, which returns our list of incidents;

```{haskell}
getData :: [Incident]
getData = getIncidents (getRows getFileContents)
```

Ok, so now let's write a function (numberIncidentsInYear) that given an integer for a year, returns the number of incidents in that year. We want to write a function that filters a list of incidents based on whether the year for those incidents is the one given, and then return the length of this list. Hence we will need another function, incidentsInYear, which takes an incident, and the year and returns a boolean value.

```{haskell}

incidentInYear :: Int -> Incident -> Bool
incidentInYear y x = y == (year x)

numberIncidentsInYear :: Int -> [Incident] -> Int
numberIncidentsInYear y i = length $ filter (incidentInYear y) i 

```

Ok, let's now set up a route, handled by the function yearRoute '/number_of_incidents/:year',  which has a parameter given by the year, and returns 'There were 'x' incidents in the year ':year'

So, to do this, we can add a new route;

```{haskell}

routes = do
  get "/" index 
  get "/number_of_incidents/:year" yearRoute  

```

And a corresponding ActionM function, which gets the route parameter with the function 'param', then returns html. To concatenate html in this sense, we can use the '<>' operator, which we import from Data.Monoid((<>))

```{haskell}

import Data.Monoid((<>))w

yearRoute :: ActionM ()
yearRoute = do 
  yr <- param "year"
  let numberIncidents = numberIncidentsInYear yr getData 
  html ( "<h5>There were " <> "incidents in the year " <> yr)
  


```