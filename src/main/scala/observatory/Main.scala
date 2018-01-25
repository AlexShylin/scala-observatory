package observatory

import java.util.Calendar

import observatory.Extraction._
import observatory.Interaction._
import observatory.Manipulation._
import observatory.Visualization2._

object Main extends App {

  //  generateMap(2015)
  generateDeviationsMap(2015)


  lazy val normals: GridLocation => Temperature = computeNormals

  def generateDeviationsMap(year: Year): Unit = {

    println(s"Started deviation generation at ${Calendar.getInstance().getTime}")
    val start = System.currentTimeMillis()

    val data = computeData(year)

    val deviations: GridLocation => Temperature = deviation(data, normals)

    def generateDeviationImageFun(y: Year, t: Tile, d: Iterable[(Location, Temperature)]): Unit = {
      val img = visualizeGrid(deviations, colors = Seq(
        (7.0, Color(0, 0, 0)),
        (4.0, Color(255, 0, 0)),
        (2.0, Color(255, 255, 0)),
        (0.0, Color(255, 255, 255)),
        (-2.0, Color(0, 255, 255)),
        (-7.0, Color(0, 0, 255))
      ), t)

      img.output(new java.io.File(s"target/deviations/$y/${t.zoom}/${t.x.toInt}-${t.y.toInt}.png"))
    }

    generateTiles(Seq((year, data)), generateDeviationImageFun)


    println(s"Generation finished at ${Calendar.getInstance().getTime}")
    println(s"Generation took ${System.currentTimeMillis() - start} ms")
  }

  def generateMap(year: Year): Unit = {

    println(s"Started generation at ${Calendar.getInstance().getTime}")

    val start = System.currentTimeMillis()

    val data = Seq((year, locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))))

    generateTiles(data, generateImageFun)

    println(s"Generation finished at ${Calendar.getInstance().getTime}")
    println(s"Generation took ${System.currentTimeMillis() - start} ms")
  }

  def computeData(year: Year): Iterable[(Location, Temperature)] = {
    locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))
  }

  def computeNormals: GridLocation => Temperature = {
    println(s"Counting normals...")
    val years = for (y <- 1975 to 1989) yield locationYearlyAverageRecords(locateTemperatures(y, "/stations.csv", s"/$y.csv"))
    val avg = average(years)
    println(s"Counted normals")
    avg
  }
}
