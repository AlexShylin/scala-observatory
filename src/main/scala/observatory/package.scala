import java.time.LocalDate

import scala.collection.GenMap

package object observatory {
  type Temperature = Double // °C, introduced in Week 1

  type Year = Int // Calendar year, introduced in Week 1
  type Day = Int
  type Month = Int

  type STN = String
  type WBAN = String
  type Stations = GenMap[(STN, WBAN), Location]

  type Temperatures = Seq[((STN, WBAN), (LocalDate, Temperature))]
}
