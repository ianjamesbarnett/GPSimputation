Directions for running the code on this example:

1. Install the GPSmobility package in R. The source for the package is included in this folder "GPSmobility_X.X.tar.gz"
2. Open command line and change the directory to this example folder
3. Enter the following command: "Rscript GPSMobility.R example1". This will create
 a. One mobility plot for each day in the data set (FlightsPlot_YYYY-MM-DD_example1.pdf)
 b. One mobility plot combined over the entirety of the data set (FlightsPlot_full_example1.pdf)
 c. A tab delimited matrix of mobility features (MobFeatMat_example1.txt). The rows of this file represent a single day in the range that the data set was collected over. Each column represents a different feature. The features are described here:
			-Hometime:			Time spent at home (minutes)	
			-DistTravelled:		Distance travelled (meters)
			-RoG:				Radius of gyration (meters)
			-MaxDiam:			Maximum diameter (longest distance between any two points) (meters)
			-MaxHomeDist:		Maximum distance from home (meters)
			-SigLocsVisited:	Number of significant locations visited that day
			-AvgFlightLen:		Average length of flights on that day (meters)
			-StdFlightLen:		Standard deviation of the length of flights on that day (meters)		
			-AvgFlightDur:		Average time flights took on that day (seconds)
			-StdFlightDur:		Standard deviation of the time flights took on that day (seconds)
			-ProbPause:			Probability (fraction of time) a person is stationary (paused) during a day (as opposed to mobile/in flight)
			-SigLocEntropy:		Location entropy across a person's significant locations for the day.
			-MinsMissing:		Number of minutes of missing data in a person's GPS mobility trace that day.
			-CircdnRtn:			Circadian routine measuring similarity in a person's locations over the course of a day to that of the other days in the data set. Numeric between 0 and 1. 1=identical routine, 0=completely different routine.
			-WkEndDayRtn:		Similar to the CircdnRtn measure, except comparisons are stratified by grouping together weekends and grouping together weekdays into two separate groups.

			
-Note that the first and last day in a dataset may contain incomplete data, which may have an effect on their mobility features. It may be advisable to ignore these days in your analysis.
