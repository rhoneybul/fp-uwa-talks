![](https://www.fpcomplete.com/hubfs/haskell_logo.svg?t=1507830794816 =100x)

## Getting Started with Haskell

Haskell is a purely functional language, and with strong static typing. The main compiler for Haskell is the Glasgow Hasell Compiler (GHC). GHC has an interactive REPL, called GHCI, which allows you to execute haskell code as well as bringing in modules. The package manager for Haskell is 'Cabal', and as you may expect, haskell modules can be installed using;

```{bash}

honeybul@ubuntu:~$ cabal install <package-name>

```

For building projects, Haskell has a pgorgram for developing haskell projects, called stack. This is very useful, and we will go over using this in a later session. Today however, we will just look at using ghc, ghci, and building a trivial program.

Some of the benefits of Haskell is that it is very difficult to have side effect in programs developed with haskell, as well as the type system, which has type inference and lazy evaluation.

### First Impressions

Haskell is a very difficult language to get your head around. I found that starting out was quite difficult, as there are few resources which give a gentle introduction to getting up and running writing fairly trivial programs.

Some of the things that have helped me, are having a basic understanding of the IO monad, and trying to use ghci as much as possible. Also, spending some time understanding how data structures (Algebraic Data Types) are made in haskell was very useful.

I do believe that doing some fairly basic projects are the best way to get started with haskell. I have spent fairly large quantities of time listening to podcasts, and reading about haskell, and nothing really helped me get going with haskell practically until i started writing some code.

### Today

Today I will go through a fairly simple exercise, where we read in a txt file, convert the file into a list of algebraic data types.

The txt file I will use will be the txt file we generated from the previous talk [Available Here](https://raw.githubusercontent.com/rhoneybul/fp-uwa-talks/master/29-09-17/practical-project/intro-project/batsmen.txt), which was the list of english batsmen, with their career statistics.

We will prompt the user to search for a player by name, and our program will return the player names and their batting averages relevant to the search term.

### Before we Begin

To install Haskell in the most minimal sense, go to [The Haskell Download Page](https://www.haskell.org/downloads#minimal)

On OSX, this is as simple as running;

```{bash}
$ brew cask install haskell-platform
```

On Linux, this is as simple as running;

```{bash}
$ sudo apt-get install haskell-platform
```

To verify that you have ghc installed;

```{bash}
honeybul@robert-aspire-ubuntu:~$ ghc -V
The Glorious Glasgow Haskell Compilation System, version 7.10.3
```

If you  have a different version to me, that shouldn't matter.

### Let's Get Started!

To get started with a hello world program, it's as simple as creating a haskell file as follows;

```{haskell}
module Main where

main = do
  putStrLn "Wello Horld!"

```

'main' is where the program runs, module 'x' let's us specify the module name.

To run this we can enter ghci, with;

```{bash}
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude>
```

Within ghci, we can run haskell code, as well as loading in modules. To load a module in the filename 'main.hs';

```{bash}
Prelude> :l main
*Main>
```

To run, we then type;

```{bash}
Prelude> main
Wello, Horld!
```

and to reload a file to the most recent version;

```{bash}
*Main> :r
Ok, modules loaded: Main.
*Main>
```

To process command line arguments, and to read files, we need to use the haskell base packages; System.IO, System.Environment. We will also need Data.List, Data.Char and  Data.List.Split for this. To import modules, we can just use the following;

```{haskell}
module Main where

import Data.List
import System.IO
import System.Environment

main = do
  putStrLn "Wello, Horld"
```

Ok, lets start with reading the file. It is worth nothing that in main, we are performing IO Actions, which are reading / writing to file, or reading and writing from the command line. Therefore, within the do statement of our program, if we are performing an IO action, we use '<-' for varible assignment, otherwise we use '='. This is a very basic explanation of how to work with MONADS (Big and Scary!).

We are going to read in the file, into a variable called contents. This part is going to perform IO, so we assign the variable using '<-'.  We will then make the file into a l ist of lists of strings, which we will call rows. We will do that in a function called getRows, which takes a String, and creates a List of List of Strings.

Note that in haskell, we can apply a function 'f' to a variable 'x', by;

```{haskell}
f x
```

We define a function, taking an input type 'x', and producing an input type 'y' as;

```{haskell}
f :: x -> y
f = do
  <do something in here, and return something of type y>
```

Let's do it!

'splitOn', takes two arguments, a character to split on, and the string.
'lines' splits a string on and end of line character
'map' a function (first argument) to a list (second argument)
So, the following applies a splitting string function on a comma, to every line!

```{haskell}

getRows :: String -> [[String]]
getRows t = do
  map (splitOn ", ") (lines t)

main = do
  contents <- readFile "batsmen.txt"
  let rows = getRows contents
  print rows
```

How easy was that!

One of the great things about using haskell, is that once you get used to it, it takes such little code to write something useful, and it's incredibly simple!

Now, let's make an 'Algebraic data type', which we can just consider to be a data structure. Let's make our rows, into a list of 'batsmen', which have a string as a name, and a float as a batting average.

We can define our ADT as follows;

```{haskell}
data Batsman = Batsman {
  name :: String,
  batting_average :: Float
} deriving (Show)
```

Note that 'deriving (Show)' just means we can print out the adt if we want.

Now, we have a list of lists, such that the first element in the list is the name, and the third element is the batting average (if we fiddle around with it a bit first!). So, let's define a function, which from a list of Strings, will give us a batsman!

We can use the 'where' keyword, to define what we want to output, and then tell the function how to get the necessary variable names!

Note, to access the nth element of a list 'x', we use x !! 0
to convert a string 'y' into a float, we can use 'read';

```{haskell}
read y :: Float
```

Let's make our function!

```{haskell}
getBatsman :: [String] -> Batsman
getBatsman x = do
  Batsman { name=n, batting_average=a } where
    n = x !! 0
    a = read (x !! 2) :: Float
```

See how we split the the average column by ':', took the second part of the split string, then read it into a float? Simple, with little code

So, lets map this function over our rows, to give us a list of batsmen!

```{haskell}
main = do
  contents <- readFile "batsmen.txt"
  let rows = getRows contents
  let bat = map getBatsman rows
  print bat  
```

Ok, now lets ask the user to search for a batsmen, then print out the name and average of the first result.

To prompt the user, 'putStrLn' a prompt, then get the line the user inputs.

```{haskell}
main = do
  putStrLn "Search for a Batsman by Name:"
  searchTerm <- readLn
```

What we will do now, is make a 'findBatsmen' function, by whether the searchTerm is contained in the lower case value of the name of the batsmen. To retrieve the name of a bastmen 'x', we can just write 'name x'. to see whether a string 'x' is in 'y', we can use isInfixOf x y, which will return a boolean value.

Note that this function should use filter, which takes two arguments, a function which returns a boolean value, and a list, and returns all elements of the list that return true to that function.

So, lets define a function 'hasName', which returns whether a string is 'in' the batsmens name. Note that a function taking two arguments x, and y can be written as;

```{haskell}
f :: x -> y -> z
```

note to convert a string to lower case, we have to map the toLower function over the string...

```{haskell}
hasName :: String -> Batsman -> Bool
hasName s b = isInfixOf s ( map toLower (name b) )
```

Now, to get our results, we can filter the function hasName, over the list of bastmen!

```{haskell}
let results = filter (hasName searchTerm) batsmen
print results
```

Now, if there are no results, we want to print 'No Batsmen found!' We can do this as follows;

```{haskell}
if length results == 0
  then print "No Batsmen Found!"
  else print results
```

And that's done!!

The whole code can be found [here](https://github.com/rhoneybul/fp-uwa-talks/tree/master/getting-started-haskell) ;

Note to compile and run this using ghc, we can run the following;

```{bash}
$ ghc -o run main.hs
$ ./run
```
