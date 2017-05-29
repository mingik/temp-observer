package observatory.mine.naive

import java.time.LocalDate
import observatory.{Location, WSID}

/**
  * Created by mingiyan.kityaev on 5/28/17.
  */
object ExtractionListsApproach {

  def toCelsius(farenheit: Double): Double = {
    (farenheit - 32d) * 5d / 9d
  }

  def avg(tmps: List[Double]): Double = tmps.sum / tmps.size.toDouble

  def locateTemperatures(year: Int, stationsLines: Iterator[String], temperaturesLines: Iterator[String]): Iterable[(LocalDate, Location, Double)] = {
    // 1. Naive List approach
    val stationsList = stationsLines.toList
    val temperaturesList = temperaturesLines.toList

    // remove empty values
    val stations = stationsList.map(line => line.split(",").toList).filterNot(stationValue => stationValue.size < 3 ||  stationValue.tail.tail.contains(""))
    val temperatures = temperaturesList.map(line => line.split(",").toList).filterNot(tempValue => tempValue.size < 3 || tempValue.tail.tail.contains(""))

    // map to types
    val stationsTyped: List[(WSID, Location)] = stations.map(stationsParts => (WSID(stationsParts(0), stationsParts(1)), Location(stationsParts(2).toDouble, stationsParts(3).toDouble)))
    val temperaturesTyped: List[(WSID, LocalDate, Double)] = temperatures.map(temperatureParts => (WSID(temperatureParts(0), temperatureParts(1)), LocalDate.of(year, temperatureParts(2).toInt, temperatureParts(3).toInt), temperatureParts(4).toDouble))

    // group by WSID
    val stationsMap: Map[WSID, List[Location]] = stationsTyped.groupBy(_._1).mapValues(_.map(_._2))
    val temperaturesMap: Map[WSID, List[(LocalDate, Double)]] = temperaturesTyped.groupBy(_._1).mapValues(_.map(li => (li._2, li._3)))

    for {
      wsid <- stationsMap.keySet.intersect(temperaturesMap.keySet)
      location: Location <- stationsMap.get(wsid).get
      dateTempList: List[(LocalDate, Double)] = temperaturesMap.get(wsid).get
      dateAvgCelcTmp: (LocalDate, Double) <- dateTempList.groupBy(_._1).mapValues(li => avg(li.map(t => toCelsius(t._2))))
    } yield (dateAvgCelcTmp._1, location, dateAvgCelcTmp._2)
  }

  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).mapValues(li => avg(li.map(dlt => dlt._3).toList))
  }
}
