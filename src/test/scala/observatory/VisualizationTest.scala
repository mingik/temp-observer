package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {


  val temp60 = Color(255, 255, 255)
  val temp32 = Color(255, 0, 0)
  val temp12 = Color(255, 255 , 0)
  val temp0 = Color(0, 255, 255)
  val tempNeg15 = Color(0, 0, 255)
  val tempNeg27 = Color(255, 0, 255)
  val tempNeg50 = Color(33, 0, 107)
  val tempNeg60 = Color(0, 0, 0)



  ignore("generate picture") {
    val stationsPath: String = "/stations.csv"
    val temperaturePath: String = "/1975-sample50k.csv"

    lazy val locateTemperatures = Extraction.locateTemperatures(1975, stationsPath, temperaturePath)
    lazy val locationYearlyAverageRecords = Extraction.locationYearlyAverageRecords(locateTemperatures)

    val palette = List(
      (60d, temp60),
      (32d, temp32),
      (12d, temp12),
      (0d, temp0),
      (-15d, tempNeg15),
      (-27d, tempNeg27),
      (-50d, tempNeg50),
      (-60d, tempNeg60)
    )

    val image = Visualization.visualize(locationYearlyAverageRecords, palette)

    image.output(new java.io.File(s"../../src/test/resources/1975-sample50k.png"))

    assert(image.pixels.size === 360 * 180)
  }

}
