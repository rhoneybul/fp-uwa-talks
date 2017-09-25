import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import scala.collection.JavaConversions._

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
     row => row.text()
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
   ).zipWithIndex

   allBatsmen.map(
     batsman => println(
      batsman._1.Name.concat(", Runs: ").concat(
         batsman._1.Runs.toString
       ).concat(", Average: ").concat(
         batsman._1.Average.toString
       ).concat(", Rank: ").concat(
         (batsman._2+1).toString
       )
     )
   )
}
