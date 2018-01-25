package observatory

import com.sksamuel.scrimage.Image

import scala.language.postfixOps

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  // maximum alpha parameter in rgba
  val alpha = 127

  /**
    * Interpolation using Inverse distance weighting.
    * More info on <a href="https://en.wikipedia.org/wiki/Inverse_distance_weighting">Wikipedia</a>
    *
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    Location.interpolateTemperature(temperatures, location)
  }

  /**
    * Linear interpolation algorithm
    * <p>
    * Representation:<br>
    * Ra       Rc   Rb<br>
    * o--------o----o
    *
    * <br>
    * Rc = (1 - t) * Ra + t * Rb<br>
    * t = dac / dab<br>
    * <br>
    * Ra - lower bound color<br>
    * Rb - upper bound color<br>
    * Rc - wanted color<br>
    * dac - distance between A && C (temperatures)<br>
    * dac - distance between A && B (temperatures)<br>
    * </p>
    *
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    assert(points.nonEmpty)
    assert(!value.isNaN)
    assert(!value.isInfinity)

    def lerp(v0: Double, v1: Double, t: Double): Int = {
      Math.round((1 - t) * v0 + t * v1).toInt
    }

    val res = points.find(_._1 == value).getOrElse {
      val sorted = points.toArray.sortBy(_._1)
      val uppers = sorted.filter(_._1 > value)
      val lowers = sorted.filter(_._1 < value)

      if (uppers.isEmpty && lowers.nonEmpty) {
        (value, lowers.last._2)
      } else if (lowers.isEmpty && uppers.nonEmpty) {
        (value, uppers.head._2)
      } else {
        val (aTemp, aColor) = uppers.head
        val (bTemp, bColor) = lowers.last
        val t = Math.abs(aTemp - value) / Math.abs(aTemp - bTemp)
        (value, Color(
          lerp(aColor.red, bColor.red, t),
          lerp(aColor.green, bColor.green, t),
          lerp(aColor.blue, bColor.blue, t)
        ))
      }
    }

    res._2
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    // generate locations
    val locations = (for (lat <- (-89 to 90).reverse; lon <- -180 to 179) yield Location(lat, lon)).toArray

    val tRoundedLoc = temperatures.par.map {
      case (l, t) => (Location(Math.round(l.lat), Math.round(l.lon)), t)
    }.seq

    val interpolatedColors = locations.par.map { l =>
      interpolateColor(colors, predictTemperature(tRoundedLoc, l))
    }

    val pixels = interpolatedColors.map(_.toPixel)

    val img = Image(360, 180, pixels.seq.toArray)

    img
  }
}

