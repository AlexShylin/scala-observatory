package observatory

import com.sksamuel.scrimage.Image

import scala.math.{ceil, floor}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    assert(point.isValid)

    val (x, y) = (point.x, point.y)

    val q00 = d00 * (1 - x) * (1 - y)
    val q01 = d01 * (1 - x) * y
    val q10 = d10 * x * (1 - y)
    val q11 = d11 * x * y
    q00 + q01 + q10 + q11
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {
    println(s"Starting grid image for $tile tile")
    val start = System.currentTimeMillis()

    val shape = 256
    val (tx, ty, z) = (tile.x.toInt, tile.y.toInt, tile.zoom)

    val locations = (for (x <- 0 until shape; y <- 0 until shape)
      yield x + y * shape -> Tile(x.toDouble / shape + tx, y.toDouble / shape + ty, z).toLocation).toArray

    val pixels = locations.par.map { case (i, l) =>
      val d00: Temperature = grid(GridLocation(l.ceil.lat.toInt, l.floor.lon.toInt))
      val d01: Temperature = grid(GridLocation(l.floor.lat.toInt, l.floor.lon.toInt))
      val d10: Temperature = grid(GridLocation(l.ceil.lat.toInt, l.ceil.lon.toInt))
      val d11: Temperature = grid(GridLocation(l.floor.lat.toInt, l.ceil.lon.toInt))

      i -> Visualization.interpolateColor(
        colors,
        bilinearInterpolation(CellPoint(l.lon - l.floor.lon, l.ceil.lat - l.lat), d00, d01, d10, d11)
      ).toPixel
    }.seq
      .sortBy(_._1)
      .map(_._2)

    println(s"Image for $tile is ready in ${System.currentTimeMillis() - start} ms")
    Image(shape, shape, pixels.toArray)
  }
}
