package weather

import org.joda.time.DateTimeZone
/**
  * Class for declaring initial information about city.
  *
  * Created by mzielinski on 20.05.16.
  */
case class City (
    name: String,
    iataCode: String,
    openWeatherMapId: Int,
    timezone: DateTimeZone,
    lon: Option[Double] = None,
    lat: Option[Double] = None
){}