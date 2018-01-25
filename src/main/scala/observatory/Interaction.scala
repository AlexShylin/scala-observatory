package observatory

import java.util.Calendar

import com.sksamuel.scrimage.Image
import observatory.Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.toLocation
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val (width, height) = (256, 256)

    val pixels = (0 until width * height).par.map(p => {
      val x = (p % width).toDouble / width + tile.x
      val y = (p / height).toDouble / height + tile.y

      p -> interpolateColor(
        colors,
        predictTemperature(
          temperatures,
          Tile(x, y, tile.zoom).toLocation
        )
      ).toPixel
    })
      .seq
      .sortBy(_._1)
      .map(_._2)

    Image(width, height, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Year, Data)], generateImage: (Year, Tile, Data) => Unit): Unit = {
    val maxZoomLevel = 3

    yearlyData.toArray.par.foreach {
      case (year, data) =>

        def generateTile(x: Double, y: Double, z: Int): Unit = {

          if (z != maxZoomLevel) {
            val nextZoom = z + 1
            generateTile(2 * x, 2 * y, nextZoom)
            generateTile(2 * x + 1, 2 * y, nextZoom)
            generateTile(2 * x, 2 * y + 1, nextZoom)
            generateTile(2 * x + 1, 2 * y + 1, nextZoom)
          }

          generateImage(year, Tile(x, y, z), data)
        }

        generateTile(0, 0, 0)
    }
  }

  def generateImageFun(y: Year, t: Tile, d: Iterable[(Location, Temperature)]): Unit = {
    val img = tile(d, colors = Seq(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    ), t)

    img.output(new java.io.File(s"target/temperatures/$y/${t.zoom}/${t.x.toInt}-${t.y.toInt}.png"))
  }

  def main(args: Array[String]): Unit = {
    println(s"Started generation at ${Calendar.getInstance().getTime}")

    import Extraction._

    val start = System.currentTimeMillis()

    val year = 2015

    val data = Seq((year, locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", "/2015.csv"))))

    generateTiles(data, generateImageFun)

    println(s"Generation finished at ${Calendar.getInstance().getTime}")
    println(s"Generation took ${System.currentTimeMillis() - start} ms")
  }
}
