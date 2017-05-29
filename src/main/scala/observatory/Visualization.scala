package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math.{acos, cos, sin, abs, pow, toRadians, atan2, sqrt, round, min, max}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val p = 3d
  val earthRadius = 6371 * 1000d // in km

  def calcDistance(loc: Location, location: Location): Double = {
    val loclat = toRadians(loc.lat)
    val loclon = toRadians(loc.lon)
    val locationlat = toRadians(location.lat)
    val locationlon = toRadians(location.lon)

    val a = pow(sin(abs(loclat - locationlat)/2), 2) + cos(loclat)*cos(locationlat)*pow(sin(abs(loclon - locationlon)/2), 2)
    earthRadius * 2 * atan2(sqrt(a), sqrt(1 - a))
  }

  def findClose(locTempDist: Iterable[(Location, Double, Double)]): Option[Double] = {
    val minLTD = locTempDist.minBy(ltd => ltd._3)
    if (minLTD._3 <= 1000d)
      Some(minLTD._2)
    else
      None
    //locTempDist.find(p => p._3 <= 1000d).map(_._2)
  }
  def calcW(d: Double) = {
    1d / pow(d, p)
  }

  val temp60 = Color(255, 255, 255)
  val temp32 = Color(255, 0, 0)
  val temp12 = Color(255, 255 , 0)
  val temp0 = Color(0, 255, 255)
  val tempNeg15 = Color(0, 0, 255)
  val tempNeg27 = Color(255, 0, 255)
  val tempNeg50 = Color(33, 0, 107)
  val tempNeg60 = Color(0, 0, 0)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    
    val locTempDist: Iterable[(Location, Double, Double)] = temperatures.map(locAndTemp => (locAndTemp._1, locAndTemp._2, calcDistance(locAndTemp._1, location)))

    findClose(locTempDist) match {
      case Some(tmp) => tmp
      case None => {
        val locTempW: Iterable[(Location, Double, Double)] = locTempDist.map(ltd => (ltd._1, ltd._2, calcW(ltd._3)))
        val num = locTempW.foldLeft(0d)((agg, ltw) => agg + ltw._2 * ltw._3)
        val dem = locTempW.foldLeft(0d)((agg, ltw) => agg + ltw._3)
        num / dem
      }
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    def bounds(c: Color): Color = {
      Color(min(max(c.red,0), 255), min(max(c.green,0), 255), min(max(c.blue,0), 255))
    }

    val sortedP = points.map(p => (abs(p._1 - value), p._1, p._2)).toSeq.sortBy(_._1)

    val p1 = sortedP.head
    val p2 = sortedP.tail.head

    if (p2._1 > abs(p2._2 - p1._2)) {
      p1._3
    } else {
      val delta = (value - p1._2) / (p2._2 - p1._2)
      bounds(
        Color(
          red = round(p1._3.red + delta * (p2._3.red - p1._3.red)).toInt,
          green = round(p1._3.green + delta * (p2._3.green - p1._3.green)).toInt,
          blue = round(p1._3.blue + delta * (p2._3.blue - p1._3.blue)).toInt
        )
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val pixels: Array[Pixel] = new Array[Pixel](360 * 180)

    for {
      x <- 0 until 360
      y <- 0 until 180
    } {
      val loc: Location = Location(90d - y, x - 180d)
      val temp = predictTemperature(temperatures, loc)
      val col: Color = interpolateColor(colors, temp)
      pixels(y * 360 + x) = Pixel(col.red, col.green, col.blue, 255)
    }
    Image(360, 180, pixels)
  }

}

