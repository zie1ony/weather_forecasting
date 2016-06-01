package weather

/**
  * Helper object for some coordinate calculations.
  *
  * Created by mzielinski on 20.05.16.
  */
object Geo {

    /**
      * Find point that is farther north.
      *
      * @param points - list of points with geographical coordinates.
      * @return
      */
    def maxNorth(points: List[Point]): Point = {
        points.reduce((a,b) => if(a.lat > b.lat) a else b)
    }

    /**
      * Find point that is farther south.
      *
      * @param points - list of points with geographical coordinates.
      * @return
      */

    def maxSouth(points: List[Point]): Point = {
        points.reduce((a,b) => if(a.lat < b.lat) a else b)
    }

    /**
      * Find point that is farther east.
      *
      * @param points - list of points with geographical coordinates.
      * @return
      */

    def maxEast(points: List[Point]): Point = {
        points.reduce((a,b) => if(a.lon > b.lon) a else b)
    }

    /**
      * Find point that is farther west.
      *
      * @param points - list of points with geographical coordinates.
      * @return
      */

    def maxWest(points: List[Point]): Point = {
        points.reduce((a,b) => if(a.lon < b.lon) a else b)
    }

    /**
      * Calculate distance between two points. It uses standard Euclidean space!
      *
      * @param a - first point.
      * @param b - second point.
      * @return - distance between points.
      */
    def distance(a: Point, b: Point): Double = {
        scala.math.sqrt(scala.math.pow(a.lon - b.lon, 2) + scala.math.pow(a.lat - b.lat, 2))
    }
}
