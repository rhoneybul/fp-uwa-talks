## Practical Introduction to Scala

### Scala

* General-Purpose
* Provides support for Functional Programming
* Strong static type system.

### First Impressions

* Gentle introduction to functional programming
* The learning curve isn't as steep as Haskell for example, as you can still utilise
familiar concepts such as classes and loops if need.
* Leaves a door open to compromise and write some imperative code to bridge a gap.

* I didn't find it particularly clear how to get started initially with a
'Hello, World!' example.
* Lots of resources I found went too deep before they covered the basic machinery

### SBT

* Interactive build tool for scala.
* Very easy to use, documentation wasn't particularly clear I found.
* Install sbt for [Windows](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Windows.html), [Linux](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html) or [Mac](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Mac.html)

To get started with a basic scala project, we make a new directory for the project;

```{bash}
~$ mkdir intro-project && cd intro-project
```

Then, to create a new scala hello-world project, we can run the following command. From my understanding, this pulls a sort of boilerplate project.

```{bash}
~/intro-project$ sbt new scala/hello-world.g8
```
When prompted, I will use the name 'intro-project'.

The main scala source file is in src/main/scala/Main.scala, and build.sbt specifies the build properties for the project. This is where we can add packages etc.

To run this, enter the interactive sbt shell;

```{bash}
~/intro-project$ cd intro-project && sbt
sbt:hello-world>
```
Then, to run the project, just type 'run' and enter;

```{bash}
sbt:hello-world> run
```
This compiles and runs the project.

### Practical Project

For this session, I am going to web-scrape the career statistics for the top 50 most successful English batsmen of all time, filter them based on their batting averages, and write them to file.

We will need to a java package, Jsoup for the webscraping. Something I have found useful at this stage, is that many java packages can be used in scala projects. To add Jsoup to the project, we need to append to the 'libraryDependencies' in build.sbt

To do this, we add the following lines in our 'build.sbt'

```{scala}

libraryDependencies += "org.jsoup" %  "jsoup" % "1.6.1",
```
To build the project again with these dependencies included, we can just run sbt in the correct directory.

To use Jsoup in our Main.scala, we have to use the following imports;

object Main is where the main code for the project gets run, and to include packages we can just use an import statement. Note that including scala.collection.JavaConversions._ is very important, for a while I didn't have this imported, and since it is a java package we are using, this caused errors and it was pretty confusing trying to figure out why.

```{scala}
// src/main/scala/Main.scala
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import scala.collection.JavaConversions._

object Main extends App {
  println("Hello, World")
}
```

The basic process for retrieving the data will be as follows;

* Given the url of the relevant data
* Then, make a get request to the url
* Parse the html and retrieve the relevant data
* Create a class bastmenStats class which contains the data relevant to each batsmen.
* Create an array of batsmen stats objects given the scraped data.
* Filter these batsmen based on batting average.
* Write these to file.

The url for the data is [here](http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;team=1;template=results;type=batting), and to retrieve this using Jsoup, we can use the following code. Note that in scala, 'val' denotes a an immutable element, and 'var' denotes a mutable element. Both are capable of type inference

```{scala}
val URL="http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;team=1;template=results;type=batting"
val document = Jsoup.connect(URL).get()
```

To get the relevant elements (the table of statistics), we can select all elements with tag 'tr', and of class 'data1'. This will basically give me an array of table row elements from the relevant table. To do this in Jsoup, we can select the relevant elements from the document we 'got';

```{scala}
val row_elements = document.select("tr.data1")
```

This will give us an array of Jsoup elements, each of which corresponds to a row in the table.

Then, to extract all the text from the rows, we can 'map' over all elements, and extract the text from the Jsoup element; The map function is one of the most useful functions in scala, and applies a function to each value in an array or list. The following says for each row in rows, apply the function on the right hand side of the equation to it. This will create a new array 'rows', where all the elements in row_elements have had the same function applied to them.

```{scala}
val rows = row_elements.map(
  row => row.text()
)
```

We are going to get each batsmens name, their highest score, and their batting average. Hence, we want to create a 'Batsmen' class. In scala, we can create a class as follows;

```{scala}

class ClassName (param1 : type, param2 : type, // etc.) {
  var classVariable1 : type = param1;
  var classVariable2 : type = param2;
  // etc.
}

```

Hence to create a Batsmen class with parameters of name runs and average, we can do the following;

```{scala}

class Batsmen (name : String, runs : Int, average : Float) {
  var Name : String = name;
  var Runs : Int = name;
  var Average : Float = name;
}

```

The constructor takes in a name, the highest score, and the average for a batsmen, and assigns these values to Name, Runs, and Average respectively. Note that in scala, when defining variable/ values, we can define them in the following manner if we want to specify the type.

```{scala}
  var x : type = value;
```

Ideally, from our array of rows, we want to extract an array of batsmen. Hence, if we have a function mapping a string (the row) to a batsman, we can just apply this function to every element in 'rows'.

To define a function in scala, we use the following syntax.

```{scala}
def function_name ( parameters ) : return_type {
  body
}
```

Our function needs to take a string, for the row, and return a batsmen.

We can get the batsman's name, by splitting the string on " ", and taking the first two values. The number of runs, is the 6th value in the split string, and the average is the 8th value in the split string.

Hence, our function will be as follows;

```{scala}
def getBatsmen (row_text : String) : Batsmen = {
  val splitted_string = row_text.split(" ")
  val name = splitted_string.slice(1, 2).mkString(" ")
  val runs = splitted_string(6).toInt
  val average = splitted_string(8).toFloat
  val batsmen = new Batsmen(name, runs, average)
  return batsmen
}
```

Note that list.slice(0, 2) is the same as list[1:2], and .mkString(" ") is the same as ' '.join() in python, or paste0 in R.

So, to get an array of Batsmen, we can apply this function (using map) over the array of rows.

```{scala}
val allBatsmen = rows.map(
  row => getBatsmen(row)
)
```

We can then use the 'filter' function to filter all the batsmen who have a batting average greater than 40. We can then zip this with index, to get the ranking of each batsmen by runs, who have an average over 40.

The filter function takes an array, and returns all those which result in a true value to the expression which is passed.

``{scala}
val batsmenOverForty = allBatsmen.filter(
  batsmen => batsmen.Average > 40
).zipWithIndex
```

To print out the players, with their rank, name, number of runs, and batting average, we can hence map a 'println' method over the array of zipped values. Since we have zipped the values, we are mapping over an array of tuples. In scala, tuples are indexed 1,...,n. Which I do not like. In this case, when we map over all elements;

```{scala}

el._1 = Batsmen Object
el._2 = Rank

```

To write this to file, we must import java.io._

```{scala}
import java.io._
```

Then we can define a print writer, which we will use to write the data to file;

```{scala}

val writer = new PrintWriter(new File("batsmen.txt"))

```

Then, we can map over the batsmenOverForty array, and write a line, describing the name, the number of runs, the average, and the rank.

```{scala}

batsmenOverForty.map(
  batsman => writer.write(
   batsman._1.Name.concat(", Runs: ").concat(
      batsman._1.Runs.toString
    ).concat(", Average: ").concat(
      batsman._1.Average.toString
    ).concat(", Rank: ").concat(
      (batsman._2+1).toString
    ).concat("\n")
  )
)

```

Note that in scala;

```{scala}
"a".concat(" b") = "a b"
```

After we have done this, we have to close the file;

```{scala}

writer.close()

```

The entire script to scrape this data, to write this to a text file is as follows;

```{scala}

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import scala.collection.JavaConversions._
import java.io._

class Batsmen (name : String, runs : Int, average : Float) {
  var Name : String = name;
  var Runs : Int = runs;
  var Average : Float = average;
}

object Main extends App {
  val URL = "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;team=1;template=results;type=batting"

  val document = Jsoup.connect(URL).get()

  val row_elements = document.select("tr.data1")

  val rows = row_elements.map(
    row => row.text
  )

  def getBatsmen (row_text : String) : Batsmen = {
    val splitted_string = row_text.split(" ")
    val name = splitted_string.slice(0, 2).mkString(" ")
    val runs = splitted_string(6).toInt
    val average = splitted_string(8).toFloat
    val batsmen = new Batsmen(name, runs, average)
    return batsmen
  }

  val allBatsmen = rows.map(
    row => getBatsmen(row)
  )

  val batsmenOverForty = allBatsmen.filter(
    batsmen => batsmen.Average > 40
  ).zipWithIndex

  val writer = new PrintWriter(new File("batsmen.txt"))

  batsmenOverForty.map(
    batsman => writer.write(
     batsman._1.Name.concat(", Runs: ").concat(
        batsman._1.Runs.toString
      ).concat(", Average: ").concat(
        batsman._1.Average.toString
      ).concat(", Rank: ").concat(
        (batsman._2+1).toString
      ).concat("\n")
    )
  )

  writer.close()

}

```
