import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import scala.collection.JavaConversions._
import java.io._

class Batsmen (name : String, runs : Int, average : Float) {
  val Name : String = name;
  val Runs : Int = runs;
  val Average: Float = average;
}

object Main extends App {

  val URL = "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;team=1;template=results;type=batting"
  val document = Jsoup.connect(URL).get()

  val row_elements = document.select("tr.data1")

  val rows = row_elements.map(
    row => row.text()
  )

  def getBatsmen ( row_text : String) : Batsmen = {
    val splitted_string = row_text.split(" ")
    val name = splitted_string.slice(0, 2).mkString(" ")
    val runs = splitted_string(6).toInt
    val average = splitted_string(8).toFloat
    val batsmen = new Batsmen(name, runs, average)
    return batsmen
  }

  val allBatsmen = rows.map(row => getBatsmen(row))

  val batsmenOverForty = allBatsmen.filter(
    batsmen => batsmen.Average > 40
  )

  val writer = new PrintWriter(new File("batsmen.txt"))

  batsmenOverForty.map(
    b => writer.write(
      b.Name.concat(", Runs: ").concat(b.Runs.toString).concat(", Average: ").concat(b.Average.toString).concat("\n")
    )
  )

  writer.close()
}
