require(gbm)
require(dplyr)

stats = read.csv("summary_stats.copy.csv")

folds = 10
fold_size = 20

fold_5 <- read.csv("5fold.csv",header=TRUE,stringsAsFactors=FALSE, na.strings="")
fold_10 <- read.csv('10fold.csv',header=TRUE,stringsAsFactors=FALSE, na.strings="")

analyze.dplyr.extract_unique_drivers <- function(df){
	return(distinct(select(df, driver_no)))
}

analyze.base.extract_unique_drivers <- function(df){
	return(unique(df$driver_no,incomparables = FALSE))
}

analyze.extract_driver_row_stats <- function(df,driver_no){
	return(df[df$driver_no == driver_no, ])
	#return(df.loc[df['driver_no'] == driver_no])
}

analyze.gbm.single_driver <- function(driver_no){
	driver_data = analyze.extract_driver_row_stats(stats,driver_no)
	analyze.gbm.per_trip_slice5(driver_data)
	#analyze.chunk(driver_data,20)
}

analyze.gbm.per_trip_slice5 <- function(driver_trips){
	#print(driver_trips)
	#driver_slices <- split(driver_trips, ceiling(seq_along(driver_trips)/fold_size))

	start_vector = fold_5[1]
	end_vector = fold_5[2]

	for (index in 1:5){
		print(index)
		start = start_vector[index]
		end = end_vector[index]

		print(start)
		print(end)

		test = filter(driver_trips, driver_trips$trip_no == start && driver_trips$trip_no == end)
		print(nrow(test))
		#test = slice(driver_trips, start:end)
		#training = driver_trips - test

		print(test)
		print("training training fjoslsjlgljsdjlvbdlv")
		#print(training)
	}
}

#RUN GBM MAIN HERE
analyze.run <- function(){
	print(typeof(stats))

	drivers_v = analyze.dplyr.extract_unique_drivers(stats)

	for (driver_no in drivers_v){
		print(driver_no)
	}
}