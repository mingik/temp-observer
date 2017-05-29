package observatory

import java.time.LocalDate

import observatory.mine.naive.ExtractionListsApproach

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsLines: Iterator[String] = try { Source.fromInputStream(getClass.getResourceAsStream(stationsFile)).getLines() } catch { case _ => Iterator.empty }
    val temperaturesLines: Iterator[String] = try { Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile)).getLines() } catch { case _ => Iterator.empty }

    ExtractionListsApproach.locateTemperatures(year, stationsLines, temperaturesLines)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ExtractionListsApproach.locationYearlyAverageRecords(records)
  }

}
