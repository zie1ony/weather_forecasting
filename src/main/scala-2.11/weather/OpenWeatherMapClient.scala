package weather

import spray.json.DefaultJsonProtocol

import scala.util.Try
import scalaj.http.Http

/**
  * Simple client for http://openweathermap.org/
  * It allows only to request weather data based on city ID in Open Weather Map.
  *
  * Created by mzielinski on 20.05.16.
  */
object OpenWeatherMapClient {

    /**
      * Case classes that represents JSON response from Open Weather Map.
      */
    case class Response(cnt: Int, list: List[CityInfo])
    case class CityInfo(coord: Coordinate, main: Conditions, id: Int, name: String)
    case class Coordinate(lon: Double, lat: Double)
    case class Conditions(temp: Double, pressure: Double, humidity: Double)

    /**
      * This object is used to bind raw JSON to above classes.
      */
    object OpenWeatherMapJsonProtocol extends DefaultJsonProtocol {
        implicit val mainFormat         = jsonFormat3(Conditions)
        implicit val coordinateFormat   = jsonFormat2(Coordinate)
        implicit val cityInfoFormat     = jsonFormat4(CityInfo)
        implicit val responseFormat     = jsonFormat2(Response)
    }

    import OpenWeatherMapJsonProtocol._
    import spray.json._

    /**
      * AppId for Open Weather Map.
      */
    private val appId = "20cf335c240564c325d879dca36183c4"

    /**
      * Base url for requesting weather data for list of cities.
      */
    private val url = "http://api.openweathermap.org/data/2.5/group?units=metric"

    /**
      * getWeatherConditions function result wrapped in Try object.
      *
      * @param cities - list of cities.
      * @return - Try object with result
      */
    def tryToGetWeatherConditions(cities: List[City]): Try[List[Point]] = {
        Try(getWeatherConditions(cities))
    }

    /**
      * Based on list of cities return list of points with
      * temperature, pressure, humidity.
      *
      * @param cities - list of cities.
      * @return - list of corresponding points.
      */
    def getWeatherConditions(cities: List[City]): List[Point] = {
        val url = getUrl(cities)
        val jsonResponse = makeCall(url)
        val response = parseJsonResult(jsonResponse)
        val points = createPoints(cities, response.list)
        points
    }

    /**
      * Build url based on list of cities.
      *
      * @param cities - list of cities.
      * @return - request url.
      */
    private def getUrl(cities: List[City]): String = {
        url + getCitiesIdsParam(cities) + getAppIdParam
    }

    /**
      * Based on cities list create GET parameter with ids
      *
      * @param cities - list of cities.
      * @return - string with GET parameter id.
      */
    private def getCitiesIdsParam(cities: List[City]): String = {
        "&id=" + cities.map(_.openWeatherMapId.toString).mkString(",")
    }

    /**
      * Use appId value to create GET parameter.
      * @return - string with GET parameter appId.
      */
    private def getAppIdParam: String = s"&appId=$appId"

    /**
      * Make a HTTP call and return response body.
      *
      * @param url - valid url address.
      * @return - body of the response
      */
    private def makeCall(url: String): String = {
        Http(url).timeout(connTimeoutMs = 5000, readTimeoutMs = 15000).asString.body
    }

    /**
      * Convert string to Response class.
      *
      * @param jsonString - string representation of JSON from Open Weather Map.
      * @return - object that represents JSON.
      */
    private def parseJsonResult(jsonString: String): Response = jsonString.parseJson.convertTo[Response]

    /**
      * Based on response from Open Weather Map and list of cities build list of Points.
      * Its assumed that both list are in the same order.
      *
      * @param cities - list of cities.
      * @param infos - list of weather conditions form API.
      * @return - list of weather points.
      */
    private def createPoints(cities: List[City], infos: List[CityInfo]) = {

        /**
          * Function that merges city with response from API and
          * produce single weather point.
          */
        val merge = (pair: (City, CityInfo)) => {
            val (city, info) = pair
            assert(city.openWeatherMapId == info.id)
            val newCity = city.copy(lon = Some(info.coord.lon), lat = Some(info.coord.lat))
            Point(
                city        = Some(newCity),
                lon         = info.coord.lon,
                lat         = info.coord.lat,
                temp        = info.main.temp,
                pressure    = info.main.pressure,
                humidity    = info.main.humidity
            )
        }

        cities.sortBy(_.openWeatherMapId).zip(infos.sortBy(_.id)).map(merge)
    }
}
