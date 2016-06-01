package weather

/**
  * This is a basic object that holds information about single point in the grid.
  *
  * Created by mzielinski on 20.05.16.
  */
case class Point (
    city: Option[City],
    lon: Double,
    lat: Double,
    temp: Double,
    pressure: Double,
    humidity: Double
) extends Ordered[Point]{

    /**
      * Create string.
      *
      * @return string representation of a point.
      */
    override def toString: String = {
        val cityName = city match {
            case None    => "unknown"
            case Some(c) => c.name
        }
        s"$cityName: long: $lon, lat: $lat, $temp C, $pressure hPa, $humidity %"
    }

    /**
      * Create CSV line.
      *
       * @return representation point as CSV line.
      */
    def toCsvLine: String = {
        s"$lon, $lat, $temp, $pressure, $humidity\n"
    }

    /**
      * Method to compare two points.
      *
      * - `x < 0` when `this < that`
      * - `x == 0` when `this == that`
      * - `x > 0` when  `this > that`
      *
      * @param that other compared point.
      * @return result of comparison as described.
      */
    override def compare(that: Point): Int = {
        (that, this) match {
            case (a, b) if a.lat < b.lat => -1
            case (a, b) if a.lat > b.lat => 1
            case (a, b) if a.lon < b.lon => -1
            case (a, b) if a.lon > b.lon => 1
            case _ => 0
        }
    }
}
