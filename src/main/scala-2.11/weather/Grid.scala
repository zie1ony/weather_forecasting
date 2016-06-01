package weather

import java.io.{File, PrintWriter}

/**
  * Created by mzielinski on 20.05.16.
  */
case class Grid (points: List[Point])

/**
  * Object for all Grid functions.
  */
object Grid {

    /**
      * Algorithm for creating a whole grid based on list of points. Each point of grid
      * represents square with dx as width and height
      *
      * @param points - list of known points.
      * @param dx - distance between interpolated points.
      * @return -
      */
    def makeGrid(points: List[Point], dx: Double): Grid = {
        /**
          * Calculate bounds of a grid.
          */
        val topNorthLat = Geo.maxNorth(points).lat.floor + 1
        val topSouthLat = Geo.maxSouth(points).lat.floor
        val topEastLon = Geo.maxEast(points).lon.floor + 1
        val topWestLon  = Geo.maxWest(points).lon.floor

        val width = topEastLon - topWestLon
        val height = topNorthLat - topSouthLat

        val makePoint = (x:Int, y:Int) => {
            Point(
                city = None,
                lon = topWestLon + x * dx,
                lat = topSouthLat + y * dx,
                temp = 0.0,
                pressure = 0.0,
                humidity = 0.0
            )
        }

        val gridPoints = for (x <- 0 to (width/dx).toInt;
                              y <- 0 to (height/dx).toInt) yield makePoint(x,y)

        val interpolatedPoints = interpolatePoints(points, gridPoints.toList, dx)

        Grid(interpolatedPoints.sorted)
    }

    /**
      * For a given list of known points interpolate values for list on unknown points.
      *
      * @param knownPoints - list of known points.
      * @param unknownPoints - list of unknown points.
      * @param dx - size of a square in the grid.
      * @return - list of interpolated points.
      */
    def interpolatePoints(knownPoints: List[Point], unknownPoints:List[Point], dx:Double): List[Point] = {
        unknownPoints.map(point => (point, kNearestPoints(4, point, knownPoints)))
                     .map((pair) => interpolatePoint(pair._1, pair._2, dx))
    }

    /**
      * Find k nearest points.
      *
      * @param k - number of points to fine.
      * @param target - target point.
      * @param list - list of known points.
      * @return - list of k nearest points.
      */
    def kNearestPoints(k: Int, target: Point, list: List[Point]):List[(Point, Double)] = {
        list.map(point => (point, Geo.distance(point, target)))
            .sortBy(_._2)
            .take(k)
    }

    /**
      * Based on list of known points interpolate values for target point by calculating weighted average
      * (closest point makes most impact). If target point is in the dx/2 range of the closest point,
      * don't interpolate - use the closest point as a result.
      *
      * @param target - point to interpolate.
      * @param knownPoints - list of known points used to interpolate.
      * @param dx - size of a square in the grid.
      * @return - interpolated point.
      */
    def interpolatePoint(target: Point, knownPoints: List[(Point, Double)], dx: Double): Point = {
        if(contains(target, knownPoints.head._1, dx)){
            knownPoints.head._1.copy(
                lon = target.lon,
                lat = target.lat
            )
        } else {
            target.copy(
                temp     = avg(knownPoints, (e:Point) => e.temp),
                pressure = avg(knownPoints, (e:Point) => e.pressure),
                humidity = avg(knownPoints, (e:Point) => e.humidity)
            )
        }
    }

    /**
      * Calculate average of list of reverse weighted list (smallest weight is better).
      *
      * @param reverseWeightedList - list of pairs (element, reverseWeight).
      * @param getter -  function for retrieve value from type T.
      * @tparam T - type of element.
      * @return - average.
      */
    def avg[T](reverseWeightedList: Iterable[(T, Double)], getter: T => Double): Double = {
        val normWeights = reverseWeights(reverseWeightedList)
        normWeights.map{case (elem, weight) => getter(elem) * weight}.sum /
            normWeights.map{case (elem, weight) => weight}.sum
    }

    /**
      * Calculate average of two elements.
      *
      * @param a - first element.
      * @param b - second element.
      * @param getter - function for retrieve value from a and b.
      * @tparam T - type of elements.
      * @return average value.
      */
    def avg[T](a: T, b: T, getter: T => Double): Double = {
        (getter(a) + getter(b)) / 2
    }

    /**
      * Function that reverts weights. Example:
      * Given: (a, 1), (b, 3), (c, 10)
      * Result: (a, 10), (b, 8), (c, 1)
      *
      * @param list - list of elements with weights
      * @tparam T - type of element.
      * @return - list of element with reversed weigts
      */
    def reverseWeights[T](list: Iterable[(T, Double)]): Iterable[(T, Double)] = {
        val maxWeight = list.maxBy(pair => pair._2)._2
        val minWeight = list.minBy(pair => pair._2)._2
        list.map{case (elem, weigth) => (elem, maxWeight+minWeight-weigth)}
    }

    /**
      * Check if knownPoint is in the square with a center in gridPoint and height and width dx.
      *
      * @param knownPoint - point that is checked.
      * @param gridPoint - point in the center of square.
      * @param dx - height and width size.
      * @return - true if point is inside the square, false otherwise.
      */
    def contains(knownPoint: Point, gridPoint: Point, dx: Double): Boolean = {
        scala.math.abs(knownPoint.lon - gridPoint.lon) < dx/2 &&
        scala.math.abs(knownPoint.lat - gridPoint.lat) < dx/2
    }

    /**
      * Save grid as a CSV file.
      *
      * @param grid - grid with points.
      * @param filename - name of CSV file.
      */
    def saveGridAsCsv(grid: Grid, filename: String) = {
        val writer = new PrintWriter(new File(filename))
        grid.points.foreach(point => writer.write(point.toCsvLine))
        writer.close()
    }

    /**
      * Simple computational fluid dynamics (CFD) algorithm. It calculates future state of point
      * by calculating average from 3 points: previous, current and next one. It makes calculations
      * in every dimensions and merge them by calculation average value.
      *
      * @param grid - grid with current state of points.
      * @param iterations - number of iteration to make.
      * @param changeRate - change rate.
      * @return - grid with state after specified number of iteration.
      */
    def forecast(grid: Grid, iterations: Int, changeRate: Double): Grid = {
        iterations match {
            case i if i < 1 => grid
            case _ => forecast(forecastNextStep(grid, changeRate), iterations - 1, changeRate)
        }
    }

    /**
      * Make single simulation step in both direction:
      *     * in latitude dimension;
      *     * in longitude dimension
      * and merge results.
      *
      * @param grid - grid with current state of points.
      * @param speed - change rate.
      * @return - future grid after one iteration
      */
    def forecastNextStep(grid: Grid, speed: Double): Grid = {
        val lonForecast = forecastOneDimension(grid, speed, _.lon)
        val latForecast = forecastOneDimension(grid, speed, _.lat)

        val result = lonForecast.zip(latForecast)
            .map{ case (lat, lon) => mergeForecastedPoints(lat, lon) }

        Grid(points = result)
    }

    /**
      * Merge two points with use of average function.
      *
      * @param a - first point to merge.
      * @param b - second point to merge.
      * @return - merged point.
      */
    def mergeForecastedPoints(a: Point, b: Point): Point = {
        assert(a.lat == b.lat)
        assert(a.lon == b.lon)

        a.copy(
            temp     = avg(a,b, (e:Point) => e.temp),
            humidity = avg(a,b, (e:Point) => e.humidity),
            pressure = avg(a,b, (e:Point) => e.pressure)
        )
    }

    /**
      * Based on dimentionGetter function separate all points that makes lines
      * and run forecastLine on them.
      *
      * @param grid - grid with current state of points.
      * @param speed - change rate.
      * @param dimentionGetter - function that helps to find points in the same line.
      * @return - future points state.
      */
    def forecastOneDimension(grid: Grid, speed: Double, dimentionGetter: Point => Double): List[Point] = {
        grid.points.groupBy(dimentionGetter(_)).toList
            .flatMap{case (value, list) => forecastLine(addBounds(list), speed)}
            .sorted
    }

    /**
      * Get every 3 points in a line and run forecastNextPoint on them.
      *
      * @param points - list of points.
      * @param speed - change rate.
      * @return - future points state.
      */
    def forecastLine(points: List[Point], speed: Double): List[Point] = {
        points.sliding(3).map{ case List(a,b,c) => forecastNextPoint(a,b,c, speed) }.toList
    }

    /**
      * Calculate future state of point. Used equation:
      *
      * futurePoint[n] = p[n] + speed * ( (avg(p[n-1], p[n], p[n+1]) - p[n] )
      *
      * @param prev - point p[n-1]
      * @param current - point p[n]
      * @param next - point[n+1]
      * @param speed - change rate.
      * @return - future point state.
      */
    def forecastNextPoint(prev: Point, current: Point, next: Point, speed: Double): Point = {
        val avgPressure = (prev.pressure + current.pressure + next.pressure) / 3
        val avgTemp     = (prev.temp     + current.temp     + next.temp)     / 3
        val avgHumidity = (prev.humidity + current.humidity + next.humidity) / 3

        current.copy(
            pressure = current.pressure + speed * (avgPressure - current.pressure),
            temp     = current.temp     + speed * (avgTemp     - current.temp),
            humidity = current.humidity + speed * (avgHumidity - current.humidity)
        )
    }

    /**
      * Duplicate bounds and add them to at the begining and at the end.
      *
      * @param points - list of points
      * @return - list of points with duplicated bounds.
      */
    def addBounds(points: List[Point]): List[Point] = List(points.head) ::: points ::: List(points.last)
}
