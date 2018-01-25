package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  def generateGrid(temperatures: Iterable[(Location, Temperature)]): Map[GridLocation, Temperature] = {
    {
      for {
        lat <- -89 to 90
        lon <- -180 to 179
      } yield GridLocation(lat, lon) -> Location.interpolateTemperature(temperatures, GridLocation(lat, lon))
    }.toMap
  }

  var cache: Map[Iterable[(Location, Temperature)], Map[GridLocation, Temperature]] = Map()

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {

    val grid: Map[GridLocation, Temperature] = cache.get(temperatures) match {
      case Some(res) => res
      case None =>
        val res = generateGrid(temperatures)
        cache += temperatures -> res
        res
    }
    (location) => grid(location)
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val gridFunctions = temperaturess.map(makeGrid)

    println("Grids created")

    (gridLocation) => {
      val temperatures = gridFunctions.par.map(_ (gridLocation))
      val avg = temperatures.sum / temperatures.size
      avg
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)

    println("Deviation function is ready")
    (location: GridLocation) => grid(location) - normals(location)
  }


}

