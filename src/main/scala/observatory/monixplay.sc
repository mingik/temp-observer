import monix.reactive._
import scala.concurrent.duration._

val tick = Observable.interval(1.second).map(_*2).take(5).dump("Out")

