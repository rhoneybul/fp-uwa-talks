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

We are going to continue on from last week, and use the batsmen data, which is available at https://raw.githubusercontent.com/rhoneybul/fp-uwa-talks/master/web-service-haskell/batsmen-data.txt.

So, let's copy over some of the logic from last week;

```{haskell}
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
```
Let's also import all the relevant modules we need from last week;

```{haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Genericsd
import Data.List
import Data.List.Split
import System.IO
import Data.Char
```

We are going to make a few changes this week, as we are going to also represent an 'id', and the 'runs scored' in the datatype. Hence, we need to change the data, and the getBatsmen functions. We will also at some point want to conver this to JSON, so we need to derive Generic as well as show. To do this, we need to include 'Language DeriveGeneric' in our script, as well as Data.Aeson (which is used for JSON), and GHC Generics. Bit of overhead I know, but in haskell importing etc is very easy, so I won't complain. So let's include the right modules first;

```{haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics
```

Now, let's modify our data type, and the getBatsman function. id is the 4th column, and the runs is the 2nd column in the file, so we can make the following changes. Also remeber, we need to derive generic for our data type. We also need to tell the program that we want to be able to convert Batsmen to JSON, which we do with 'instance ToJSON Batsmen'.

```{haskell}

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
```


```{haskell}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

data Incident = Incident {
  year :: Int, 
  incidentType :: String, 
  location :: String
} deriving (Show, Generic)

```

Firstly, let's read in the file, with unsafePerformIO (which to use we need to import System.IO.Unsafe, and System.IO). This function getFileContents, gets a string, with the entire file contents.

```{haskell}
import System.IO.Unsafe 
import System.IO

getFileContents :: String
getFileContents = unsafePerformIO (readFile "batsmen-data.txt")
```

Now, we have the entire file, as a string. As with last week, we can use our getRows function, to convert the string into rows, then map our getBatsmen function over these Rows as a list of Batsmen. So, to simplify things, lets just write a function that gets the list of batsmen

```{haskell}
allBatsmen :: [Batsmen]
allBatsmen = map getBatsmen (getRows getFileContents)
```

For me, this really solidifies the point that EVERYTHING IS A FUNCTION. allBatsmen is a list of Batsmen, however, this is also a function that does some io, and processing.

Lets just test this out really quickly by changing our main function;

```{haskell}

main = print allBatsmen

``` 
Nice!

So, instead of this, lets create a route, all batsmen, which returns this list of datatypes, in json form!

```{haskell}

routes :: ScottyM ()
routes = do 
  get "/" index 
  get "/all/" returnAllBatsmen

returnAllBatsmen :: ActionM ()
returnAllBatsmen = json (allBatsmen)

```

Let's try this out!

Now, let's make a route, where we can search for a batsman by name. The function this route calls, should run the batsmanSearch function, with a search string over the list of batsmen, and then return a json object. So let's write this function! NOte that param is a function in scotty, which returns the specified parameter from a route. We also need a batsmanSearch function, which takes a search string, and the list of batsmen and returns the list of batsmen whose name matches the search string.

```{haskell}

batsmanSearch :: String -> [Batsmen] -> [Batsmen]
batsmanSearch s b = filter (hasName s) b

findByName :: ActionM ()
findByName = do
    searchString <- param "sstring"
    let results = batsmanSearch searchString allBatsmen
    json $ results
```

Now, let's create a route for this;

```{haskell}

routes :: ScottyM ()
routes = do
    get "/" index 
    get "/all/" returnAllBatsmen
    get "/search/:sstring" findByName

```

and let's test it out!

Now, finally let's add a route where we can find a batsman by his id. Firstly lets define a function that takes an integer batsmen, and returns a boolean value for whether the batsmens id equals that integer 

```{haskell}
hasId :: Int -> Batsmen -> Bool
hasId i b = i == (_id b) 
```

Now, lets write a funciton, that takes an integer id and a  list of batsmen and filters this list, so that only the batsmen with that particular id is returned. Note that since we are using filter, we need to return a list, since this is what filter will output

```{haskell}
idSearch :: Int -> [Batsmen] -> [Batsmen]
idSearch i b = filter (hasId i) b
```

Now, let's define our route function, which takes the id we are looking for as a parameter, and returns a json object of the batsmen which matches that id 

```{haskell}
findById :: ActionM ()
findById = do
  _id <- param "id"
  let results = idSearch _id allBatsmen
  json $ results
``` 

Now we need to add a route for this!

```{haskell}
routes :: ScottyM ()
routes = do
    get "/" index 
    get "/all/" returnAllBatsmen
    get "/search/:sstring" findByName
    get "/batsman/:id" findById
```