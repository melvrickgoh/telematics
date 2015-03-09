require(gbm)
require(dplyr)

stats = read.csv("summary_stats.1000.csv")

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

analyze.extract_anti_driver_row_stats <- function(df,driver_no,numberOfTrips){
	anti_frame = df[df$driver_no != driver_no, ]
	negative_trips = anti_frame[sample(nrow(anti_frame), numberOfTrips), ]
	negative_trips = mutate(negative_trips, belongs_to_driver = 0)
	#print(negative_trips[,c("trip_no","driver_no","belongs_to_driver")])
	return(negative_trips)
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

	start_vector = fold_5$start
	negative_trips = analyze.extract_anti_driver_row_stats(stats,driver_no,160)

	for (i in start_vector){
		start = i
		end = i+39

		test = filter(driver_trips, driver_trips$trip_no >= start & driver_trips$trip_no <= end)
		training = filter(driver_trips, driver_trips$trip_no < start | driver_trips$trip_no > end)

		analyze.gbm.run_trip_predictions(training,test,negative_trips)
	}
	#print(stats[,c("trip_no","driver_no","cur_min","cur_max")])
	#print(negative_trips$trip_no)
}

analyze.gbm.other_driver_trips <- function(){

}

analyze.gbm.run_trip_predictions <- function(train,test,negative_trips){
	#add class label to train
	train = mutate(train, belongs_to_driver = 1)
	#print(train$is_driver)
	super_train = rbind(train,negative_trips)
	#print(super_train[,c("trip_no","driver_no","belongs_to_driver")])
	
	#remove the belongs to driver column
	belongs_to_driver_group = super_train$belongs_to_driver
	super_train = select(super_train, -belongs_to_driver)

	end_train = nrow(super_train)
	
	all = rbind(super_train,test)
	end_all = nrow(all)

	all = select(all,
		speed_5_pcnt,
		speed_10_pcnt,
		speed_15_pcnt,
		speed_20_pcnt,
		speed_25_pcnt,
		speed_30_pcnt,
		speed_35_pcnt,
		speed_40_pcnt,
		speed_45_pcnt,
		speed_50_pcnt,
		speed_55_pcnt,
		speed_60_pcnt,
		speed_65_pcnt,
		speed_70_pcnt,
		speed_75_pcnt,
		speed_80_pcnt,
		speed_85_pcnt,
		speed_90_pcnt,
		speed_95_pcnt,
		speed_100_pcnt,
		speed_mean,
		speed_min,
		speed_max,
		speed_variance,
		speed_std,
		tang_accel_5_pcnt,
		tang_accel_10_pcnt,
		tang_accel_15_pcnt,
		tang_accel_20_pcnt,
		tang_accel_25_pcnt,
		tang_accel_30_pcnt,
		tang_accel_35_pcnt,
		tang_accel_40_pcnt,
		tang_accel_45_pcnt,
		tang_accel_50_pcnt,
		tang_accel_55_pcnt,
		tang_accel_60_pcnt,
		tang_accel_65_pcnt,
		tang_accel_70_pcnt,
		tang_accel_75_pcnt,
		tang_accel_80_pcnt,
		tang_accel_85_pcnt,
		tang_accel_90_pcnt,
		tang_accel_95_pcnt,
		tang_accel_100_pcnt,
		tang_accel_mean,
		tang_accel_min,
		tang_accel_max,
		tang_accel_variance,
		tang_accel_std,
		norm_accel_5_pcnt,
		norm_accel_10_pcnt,
		norm_accel_15_pcnt,
		norm_accel_20_pcnt,
		norm_accel_25_pcnt,
		norm_accel_30_pcnt,
		norm_accel_35_pcnt,
		norm_accel_40_pcnt,
		norm_accel_45_pcnt,
		norm_accel_50_pcnt,
		norm_accel_55_pcnt,
		norm_accel_60_pcnt,
		norm_accel_65_pcnt,
		norm_accel_70_pcnt,
		norm_accel_75_pcnt,
		norm_accel_80_pcnt,
		norm_accel_85_pcnt,
		norm_accel_90_pcnt,
		norm_accel_95_pcnt,
		norm_accel_100_pcnt,
		total_accel_5_pcnt,
		total_accel_10_pcnt,
		total_accel_15_pcnt,
		total_accel_20_pcnt,
		total_accel_25_pcnt,
		total_accel_30_pcnt,
		total_accel_35_pcnt,
		total_accel_40_pcnt,
		total_accel_45_pcnt,
		total_accel_50_pcnt,
		total_accel_55_pcnt,
		total_accel_60_pcnt,
		total_accel_65_pcnt,
		total_accel_70_pcnt,
		total_accel_75_pcnt,
		total_accel_80_pcnt,
		total_accel_85_pcnt,
		total_accel_90_pcnt,
		total_accel_95_pcnt,
		total_accel_100_pcnt,
		cur_5_pcnt,
		cur_10_pcnt,
		cur_15_pcnt,
		cur_20_pcnt,
		cur_25_pcnt,
		cur_30_pcnt,
		cur_35_pcnt,
		cur_40_pcnt,
		cur_45_pcnt,
		cur_50_pcnt,
		cur_55_pcnt,
		cur_60_pcnt,
		cur_65_pcnt,
		cur_70_pcnt,
		cur_75_pcnt,
		cur_80_pcnt,
		cur_85_pcnt,
		cur_90_pcnt,
		cur_95_pcnt,
		cur_100_pcnt,
		cur_mean,
		cur_min,
		cur_max,
		cur_variance,
		cur_std,
		distance,
		distance_mean,
		distance_min,
		distance_max,
		distance_variance,
		distance_std)
	#variables for modelling

	head(all)

	#THE MODEL
	#as always, look at the help page for the function
	?gbm

	#a high guess of how many trees we'll need
	ntrees = 5000


	#how to tune parameters? 
	#in this video, we'll tune the number of trees and 
	#use reasonable values of other parameters
	#test different parameters with Cross Validation 
	#see the other video on this topic


	Model = gbm.fit( 
	  x = all[1:end_train,] #dataframe of features
	  , y = belongs_to_driver_group #dependent variable
	  #two ways to fit the model
	  #use gbm.fit if you are going to specify x = and y = 
	  #instead of using a formula
	  #if there are lots of features, I think it's easier to specify 
	  #x and y instead of using a formula
	  
	  
	  , distribution = "bernoulli"
	  #use bernoulli for binary outcomes
	  #other values are "gaussian" for GBM regression 
	  #or "adaboost"
	  
	  
	  , n.trees = ntrees
	  #Choose this value to be large, then we will prune the
	  #tree after running the model
	  
	  
	  , shrinkage = 0.01 
	  #smaller values of shrinkage typically give slightly better performance
	  #the cost is that the model takes longer to run for smaller values
	  
	  
	  , interaction.depth = 3
	  #use cross validation to choose interaction depth!!
	  
	  
	  , n.minobsinnode = 10
	  #n.minobsinnode has an important effect on overfitting!
	  #decreasing this parameter increases the in-sample fit, 
	  #but can result in overfitting
	  
	  , nTrain = round(end_train * 0.8)
	  #use this so that you can select the number of trees at the end
	  
	  # , var.monotone = c() 
	  #can help with overfitting, will smooth bumpy curves
	  
	  , verbose = FALSE #print the preliminary output
	)  
	    
	  

	  



	#look at the last model built
	#Relative influence among the variables can be used in variable selection
	summary(Model)
	#If you see one variable that's much more important than all of the rest,
	#that could be evidence of overfitting.

	#optimal number of trees based upon CV
	gbm.perf(Model)

	#look at the effects of each variable, does it make sense?
	?plot.gbm
	for(i in 1:length(Model$var.names)){
	  plot(Model, i.var = i
	       , ntrees = gbm.perf(Model, plot.it = FALSE) #optimal number of trees
	       , type = "response" #to get fitted probabilities
	       )
	}

	###########################################









	################ Make predictions ##################
	#test set predictions
	TestPredictions = predict(object = Model,newdata =all[(end_train+1):end_all,]
	                          , n.trees = gbm.perf(Model, plot.it = FALSE)
	                          , type = "response") #to output a probability
	#training set predictions
	TrainPredictions = predict(object = Model,newdata =all[1:end_train,]
	                           , n.trees = gbm.perf(Model, plot.it = FALSE)
	                           , type = "response")


	#round the predictions to zero or one
	#in general, don't do this!
	#it was only because the answers in the comp had to be 0 or 1
	TestPredictions = TestPredictions
	TrainPredictions = TrainPredictions
	#could also mess around with different cutoff values
	#would need CV to determine the best


	head(TrainPredictions, n = 20)
	head(belongs_to_driver_group, n = 20)



	#in sample classification accuracy
	1 - sum(abs(belongs_to_driver_group - TrainPredictions)) / length(TrainPredictions) 

	#write the submission
	# paste(data$F, data$E, data$D, data$C, sep="_")
	# 1:nrow(test)
	submission = data.frame(driver_trip = paste(test$driver_no, test$trip_no, sep="_"), prob = TestPredictions)
	write.table(submission, file = "submission.csv", row.names = FALSE, col.names = FALSE, quote=FALSE, append=TRUE, sep=",")

}

#RUN GBM MAIN HERE
analyze.run <- function(){
	drivers_v = analyze.dplyr.extract_unique_drivers(stats)

	for (driver_no in drivers_v){
		analyze.gbm.single_driver(driver_no)
	}
}