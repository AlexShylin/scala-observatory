package observatory

import java.nio.file.Paths
import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  //////////////
  // stations //
  //////////////

  var stationsDataCache: Stations = _
  var stationsFileCache: String = _

  /** If data is already computed - returns it. Checks equality by filename.
    *
    * @return Stations location data mapped to (stn id, wban id) */
  def stations(stationsFile: String): Stations = {
    assert(stationsFile.nonEmpty)

    if (stationsFileCache == stationsFile) stationsDataCache
    else {
      val lines = readLines(stationsFile)
      val stationsExisting = lines.toArray.par.filterNot(_ endsWith ",")
      val stations = stationsExisting.map(l => {
        val fields = l.split(",")
        Station(fields(0), fields(1), Location(fields(2).toDouble, fields(3).toDouble))
      })

      val res = stations.groupBy(s => (s.stnId, s.wbanId)).mapValues(v => v.head.loc)

      stationsFileCache = stationsFile
      stationsDataCache = res

      res.seq
    }
  }

  /////////////////
  // temperature //
  /////////////////

  def temperaturesDataByYear(year: Year, temperaturesFile: String): Temperatures = {
    assert(temperaturesFile.nonEmpty)

    val lines = readLines(temperaturesFile).toArray
    val weatherData = lines.par.map(l => {
      val fields = l.split(",")
      StationTemperature(
        fields(0),
        fields(1),
        fields(2).toInt,
        fields(3).toInt,
        Math.round(fields(4).toDouble)
      )
    })

    val res = weatherData.groupBy(s => (s.stnId, s.wbanId)).toSeq.map {
      case (k, v) =>
        val d = v.head
        (k, (LocalDate.of(year, d.month, d.day), toCelsius(d.temperature)))
    }

    res.seq
  }

  private def toCelsius(temperature: Temperature): Temperature = (temperature - 32) * 5 / 9

  //////////////////
  // to implement //
  //////////////////

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationMap = stations(stationsFile)
    val yearData = temperaturesDataByYear(year, temperaturesFile)

    val temperatures = stationMap.par.flatMap {
      case (ids, loc) =>
        val s = yearData.filter(_._1 == ids)
        s.map { case (_, v) =>
          (v._1, loc, v._2)
        }
    }

    println(s"Data for $year fetched")
    temperatures.seq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val avg = records.groupBy {
      case (_, loc, _) => loc
    }.mapValues(seq => seq.foldLeft(0d)(_ + _._3) / seq.size)

    avg
  }

  ///////////////////////
  // file util methods //
  ///////////////////////

  def readLines(file: String): Iterator[String] = {
    Source.fromFile(fsPath(file)).getLines()
  }

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

}
