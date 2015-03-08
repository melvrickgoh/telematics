source("packages.R",echo = TRUE)
source("trip_metrics.R",echo = TRUE)
source("helpers.R", echo = TRUE)

current_directory = getwd()
parent_directory = dirname(current_directory)
drivers_directory = p(parent_directory,"/drivers_sub") #see helpers.R for concat of strings

#for parsing through all drivers data
#Uncomment to run all drivers
proc.all_drivers(drivers_directory)

#Uncomment to do a specific driver
#proc.driver_trips("C:/Users/Melvrick/Desktop/Data Mining/drivers/1003")

#Uncomment to target selected folders (i.e. drivers)
#target_directory = p(parent_directory,"/drivers_sub")
#proc.selective_driver_trips(drivers_directory,"remaining_files.txt")