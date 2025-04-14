## Supplemental code for "A framework to quantify microclimate modulation using average, variability, and extremes" (submitted for review)
In this study, we compiled and tested methods to quantify microclimate modulation, using a framework of average, variability, and extreme modulation.

### Workflow (folder /scripts):
- **0-xxx.R** files contain the functions necessary to create the simulations and calculate the microclimate indices
- **1-xxx.R** files contain code for accessing publicly available data from NOAA and sampling sites with the appropriate characteristics (hourly data in the year 2020)
- **2-microclimate_simulation.R** uses our functions to simulate microclimates with different characteristics from the NOAA data
- **3-xxx.R** files contain code to test the functionality of the simulation itself
- **4-indices_calculation.R** uses our functions to calculate microclimate indices for all simulated microclimates
- **5-xxx.R** files contain the code to calculate and plot the quality metrics of the applied indices
- **6-quantile_effects.R** calculates the indices and quality metrics for different levels of quantile-dependent indices
- **7-mydiv_data_application.R** calculates the best-performing indices for an example of real micro- and macroclimate data

### Data (folder /data):
- **1-noaa** contains the data downloaded from NOAA
- **2-microclimate-simulations** contains the simulated microclimates
- **3-indices** contains the indices calculated for the simulated microclimates
- **4-index-performance** contains the quality metrics calculated for the indices
- **5-example-data** contains the data for the example application (MyDiv)
