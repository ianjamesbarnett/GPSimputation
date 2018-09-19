Directions for running the code on this example:

1. Install the GPSmobility package in R. The source for the package is included in this folder "GPSmobility_X.X.tar.gz"
2. Open command line and change the directory to this example folder
3. Enter the following command: "Rscript GPSMobility.R example1". This will create
	* One mobility plot for each day in the data set (FlightsPlot_YYYY-MM-DD_example1.pdf)
	* One mobility plot combined over the entirety of the data set (FlightsPlot_full_example1.pdf)
	* A tab delimited matrix of mobility features (MobFeatMat_example1.txt). The rows of this file represent a single day in the range that the data set was collected over. Each column represents a different feature. The features are described here:
		1. Hometime:			Time spent at home (minutes)	
		2. DistTravelled:		Distance travelled (meters)
		3. RoG:				Radius of gyration (meters)
		4. MaxDiam:			Maximum diameter (longest distance between any two points) (meters)
		5. MaxHomeDist:		Maximum distance from home (meters)
		6. SigLocsVisited:	Number of significant locations visited that day
		7. AvgFlightLen:		Average length of flights on that day (meters)
		8. StdFlightLen:		Standard deviation of the length of flights on that day (meters)		
		9. AvgFlightDur:		Average time flights took on that day (seconds)
		10. StdFlightDur:		Standard deviation of the time flights took on that day (seconds)
		11. ProbPause:			Probability (fraction of time) a person is stationary (paused) during a day (as opposed to mobile/in flight)
		12. SigLocEntropy:		Location entropy across a person's significant locations for the day.
		13. MinsMissing:		Number of minutes of missing data in a person's GPS mobility trace that day.
		14. CircdnRtn:			Circadian routine measuring similarity in a person's locations over the course of a day to that of the other days in the data set. Numeric between 0 and 1. 1=identical routine, 0=completely different routine.
		15. WkEndDayRtn:		Similar to the CircdnRtn measure, except comparisons are stratified by grouping together weekends and grouping together weekdays into two separate groups.

			
 * Note that the first and last day in a dataset may contain incomplete data, which may have an effect on their mobility features. It may be advisable to ignore these days in your analysis.
