package observatory.mine.naive

import java.io.{BufferedReader, InputStreamReader}
import java.time.LocalDate

import monix.reactive.Observable
import monix.reactive.observables.GroupedObservable
import observatory.mine.naive.ExtractionListsApproach.avg
import observatory.{Location, WSID}

/**
  * Created by mingiyan.kityaev on 5/28/17.
  */
class ExtractionMonixApproach {
  case class DateLocationTemps(var date: LocalDate, var location: Location, var temps: List[Double])
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val stationsObs: Observable[(WSID, DateLocationTemps)] = Observable
      .fromLinesReader(new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(stationsFile))))
      .filter(line => {
        val parts = line.split(",")
        (parts.size == 4) && (parts(2) != "" || parts(3) != "")
      })
      .map(line => {
        val parts = line.split(",")
        (WSID(parts(0), parts(1)), DateLocationTemps(null, Location(parts(2).toDouble, parts(3).toDouble), List.empty))
      })

    val temperaturesObs: Observable[(WSID, DateLocationTemps)] = Observable
      .fromLinesReader(new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(temperaturesFile))))
      .filter(line => {
        val parts = line.split(",")
        (parts.size == 5) && (parts(2) != "" || parts(3) != "" || parts(4) != "" || parts(4) != "9999.9")
      })
      .map(line => {
        val parts = line.split(",")
        (WSID(parts(0), parts(1)), DateLocationTemps(LocalDate.of(year, parts(2).toInt, parts(3).toInt), null, List(parts(4).toDouble))
      })

    val all: Observable[(WSID, DateLocationTemps)] = Observable.merge(stationsObs, temperaturesObs)

    ???
  }

  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).mapValues(li => avg(li.map(dlt => dlt._3).toList))
  }
}
