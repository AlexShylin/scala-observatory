package observatory

import com.sksamuel.scrimage.Pixel

abstract class AbstractLocation {
  val lat: Double
  val lon: Double

  // Earth radius, km
  protected val r = 6371d

  def greatCircleDistance(that: AbstractLocation): Double = {
    import Math._
    if (this.lat == that.lat && this.lon == that.lon)
      0
    else {
      val (thisRadLat, thisRadLon) = (toRadians(this.lat), toRadians(this.lon))
      val (thatRadLat, thatRadLon) = (toRadians(that.lat), toRadians(that.lon))
      val arccos = acos(sin(thisRadLat) * sin(thatRadLat) + cos(thisRadLat) * cos(thatRadLat) * cos(abs(thatRadLon - thisRadLon)))
      r * arccos
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[AbstractLocation]

  override def equals(other: Any): Boolean = other match {
    case that: AbstractLocation =>
      (that canEqual this) &&
        lat == that.lat &&
        lon == that.lon
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(lat, lon)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Location {
  // IDW power parameter
  val p = 6

  def getWeight(baseLoc: AbstractLocation, anotherLoc: AbstractLocation): Double = {
    import Math._

    val distance = baseLoc.greatCircleDistance(anotherLoc)
    if (distance.isNaN || distance == 0) 1 / pow(PI, p)
    else 1 / pow(distance, p)
  }

  def interpolateTemperature
  (
    temperatures: Iterable[(AbstractLocation, Temperature)],
    location: AbstractLocation
  ): Temperature = {

    val tupleRes = temperatures.find(location == _._1).getOrElse {

      val (sumTemperaturesWeights, sumWeights) = temperatures.par.aggregate((0.0d, 0.0d))(
        {
          case ((stw, sw), (loc, t)) =>
            val w = Location.getWeight(location, loc)
            (stw + w * t, sw + w)
        }, {
          case ((stwA, swA), (stwB, swB)) => (stwA + stwB, swA + swB)
        })

      val interpolatedValue = sumTemperaturesWeights / sumWeights

      (location, interpolatedValue)
    }

    tupleRes._2
  }
}

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) extends AbstractLocation {

  /**
    * Rounds to lower anyway, e.g. (8.3, 8.7) => (8, 8)
    */
  lazy val ceil: GridLocation = {
    val lon = if (this.lon > 179) -180 else if (this.lon < -180) 179 else this.lon
    GridLocation(Math.ceil(lat), Math.ceil(lon))
  }

  /**
    * Rounds to upper anyway, e.g. (8.3, 8.7) => (9, 9)
    */
  lazy val floor: GridLocation = {
    val lon = if (this.lon > 179) -180 else if (this.lon < -180) 179 else this.lon
    GridLocation(Math.floor(lat), Math.floor(lon))
  }
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  *
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Double, lon: Double) extends AbstractLocation

/**
  * For week 1. Represents weather station.
  *
  * @param stnId  STN identifier
  * @param wbanId WBAN identifier
  * @param loc    Location of latitude and longitude
  */
case class Station(stnId: STN, wbanId: WBAN, loc: Location)

/**
  *
  * @param stnId       STN identifier
  * @param wbanId      WBAN identifier
  * @param month       month number
  * @param day         day number
  * @param temperature temperature in Celsius
  */
case class StationTemperature(stnId: STN, wbanId: WBAN, month: Month, day: Day, temperature: Temperature)

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  *
  * @param x    X coordinate of the tile
  * @param y    Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Double, y: Double, zoom: Int) {

  import scala.math._

  lazy val toLocation = Location(
    toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
    x.toDouble / (1 << zoom) * 360.0 - 180.0)
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  *
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double) {
  def isValid: Boolean = x >= 0 && x <= 1 && y >= 0 && y <= 1
}

/**
  * Introduced in Week 2. Represents an RGB color.
  *
  * @param red   Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue  Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  def toPixel = Pixel(red, green, blue, Visualization.alpha)
}

