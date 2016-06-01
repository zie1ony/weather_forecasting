package weather

import org.joda.time.{DateTime, DateTimeZone}

import scala.util.Success
import scala.util.Failure

/**
  * Main object that runs all the code.
  *
  * Created by mzielinski on 20.05.16.
  */
object Main extends App {

    /**
      * Init function. It tries to load data and run process function
      * if everything is fine.
      */
    private def init() = {
        val syndeyTZ = DateTimeZone.forID("Australia/Sydney")
        val cities = List(
            City("Adelajda" , "ADL" , 2078025, DateTimeZone.forID("Australia/Adelaide")),
            City("Albury"   , ""    , 2178174, syndeyTZ),
            City("Ballarat" , ""    , 2177091, syndeyTZ),
            City("Bendigo"  , ""    , 2176187, syndeyTZ),
            City("Melbourne", "MEL" , 2158177, syndeyTZ),
            City("Morwell"  , ""    , 2156825, syndeyTZ),
            City("Canberra" , ""    , 8029780, syndeyTZ),
            City("Sydney"   , "SYD" , 2147714, syndeyTZ),
            City("Mildura"  , "MQL" , 2157698, syndeyTZ),
            City("Griffith" , "GFF" , 2164422, syndeyTZ)
        )

        OpenWeatherMapClient.tryToGetWeatherConditions(cities) match {
            case Failure(ex)                => println("Cannot load data from remote server. Reason: " + ex.getMessage)
            case Success(stationsPoints)    => process(stationsPoints, new DateTime(DateTimeZone.UTC));
        }
    }

    /**
      * To predict future weather condition from a list of stations:
      *     1. Interpolate known points to have grid of points
      *     2. Run simple computational fluid dynamics algorithm (CFD)
      *
      * Values of parameters: dx = 0.1, iterations = 100, changeRate = 0.3
      * were determined experimentally to reflect 1 hour change.
      *
      * @param stationsPoints - list on known weather conditions.
      * @param timeOfMeasureUTC - time of measurement.
      */
    private def process(stationsPoints: List[Point], timeOfMeasureUTC: DateTime) = {
        val grid       = Grid.makeGrid(stationsPoints, dx = 0.1)
        val futureGrid = Grid.forecast(grid, iterations = 100, changeRate = 0.3)
        val resultPoints = filterPointsWithIataCode(futureGrid.points)
        print(resultPoints, timeOfMeasureUTC.plusHours(1))

        Grid.saveGridAsCsv(grid, "currentGrid.csv")
        Grid.saveGridAsCsv(futureGrid, "oneHourGrid.csv")
    }

    /**
      * Filter points that has IATA codes.
      *
      * @param points - list of points with or without city information.
      * @return - list of points with IATA codes.
      */
    private def filterPointsWithIataCode(points: List[Point]): List[Point] = {
        points.filter{p => p.city match {
            case None => false
            case Some(city) => city.iataCode != ""
        }}
    }

    /**
      * Print weather conditions.
      *
      * @param list - list of points. Make sure that all of them have city information.
      * @param dateUTC - time to print.
      */
    private def print(list: List[Point], dateUTC: DateTime): Unit = {
        list.foreach(point => println(makeLine(point, dateUTC)))
    }

    /**
      * Based on the point and time create line with forecast.
      *
      * @param point - single point with weather conditions.
      * @param date - time to print.
      * @return - line with weather forecast.
      */
    private def makeLine(point: Point, date:DateTime): String = {
        val city = point.city.get
        val localDate = date.withZone(city.timezone).toLocalDateTime
        val tempSign = if(point.temp > 0) "+" else ""
        List(
            city.iataCode,
            s"${round2(city.lat.get)},${round2(city.lon.get)}",
            localDate,
            s"$tempSign${round1(point.temp)}",
            round1(point.pressure),
            round0(point.humidity)
        ).mkString("|")
    }

    /**
      * Round function with use of currying.
      *
      * @param points - number of decimal points to print
      * @param value - value to round
      * @return - string with rounded value
      */
    private def round(points: Int)(value: Double): String = ("%."+points+"f").format(value).replaceAll(",", ".")
    private def round0(value: Double): String = round(0)(value)
    private def round1(value: Double): String = round(1)(value)
    private def round2(value: Double): String = round(2)(value)

    init()
}
