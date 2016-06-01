# Weather Forecasting

This is a simple application for a weather forecasting.

## What does it do?

1. Load weather data (temperature, pressure, humidity) from http://openweathermap.org/ for few stations in South Eastern Australia. 
2. Create a map (grid) of South Eastern Australia.
3. Interpolate weather conditions for every point on the map based on loaded data.
4. Run simple CFD (computational fluid dynamics) algorithm to predict future weather conditions.
5. Print results.

## Simplifications

1. Distance between points is calculated with assumption that Earth is flat.
2. Heat, humidity and pressure transfer is calculated with usage of simple averages.
3. Altitude information is not used.
4. Cloud cover is not used.

## Run

    sbt run
 
## Results

After running a program two files should appear: `currentGrid.csv` and `oneHourGrid.csv`. First represents grid in initial state, second after forecasting.
Repository contains file `visualizeResults.ipynb`. This is IPython Notebook that visualize above files.

### Temperature

![Current Temperature](/plots/Current_Temperature.jpg?raw=true "Current Temperature")
![Future Temperature](/plots/Future_Temperature.jpg?raw=true "Future Temperature")
![Temperature Change](/plots/Temperature_Change.jpg?raw=true "Temperature Change")

### Pressure

![Current Pressure](/plots/Current_Pressure.jpg?raw=true "Current Pressure")
![Future Pressure](/plots/Future_Pressure.jpg?raw=true "Future Pressure")
![Pressure Change](/plots/Pressure_Change.jpg?raw=true "Pressure Change")

### Humidity

![Current Humidity](/plots/Current_Humidity.jpg?raw=true "Current Humidity")
![Future Humidity](/plots/Future_Humidity.jpg?raw=true "Future Humidity")
![Humidity Change](/plots/Humidity_Change.jpg?raw=true "Humidity Change")
