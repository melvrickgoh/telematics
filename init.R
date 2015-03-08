source("packages.R",echo = FALSE)
source("trip_metrics.R",echo = FALSE)
source("helpers.R", echo = FALSE)
source("gbm.R", echo = FALSE)

current_directory = getwd()
parent_directory = dirname(current_directory)
drivers_directory = p(parent_directory,"/drivers_sub") #see helpers.R for concat of strings

#for parsing through all drivers data
#Uncomment to run all drivers
#proc.all_drivers(drivers_directory)

#Uncomment to do a specific driver
#proc.driver_trips("C:/Users/Melvrick/Desktop/Data Mining/drivers/1003")

#Uncomment to target selected folders (i.e. drivers)
#target_directory = p(parent_directory,"/drivers")
#proc.selective_driver_trips(target_directory,"remaining_files.txt")


#UNCOMMENT TO RUN GBM. ONLY RUN AFTER YOU HAVE PROCESSED THE DATASET
#CHECK gbm.R for variable setting

#RUN ALL
#analyze.run()

#RUN 1 driver
analyze.gbm.single_driver(1000)