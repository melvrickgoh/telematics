source("packages.R",echo = TRUE)
source("trip_metrics.R",echo = TRUE)
source("helpers.R", echo = TRUE)

current_directory = getwd()
parent_directory = dirname(current_directory)
drivers_directory = p(parent_directory,"/drivers") #see helpers.R for concat of strings

#for parsing through all drivers data
#proc.all_drivers(drivers_directory)

proc.driver_trips("C:/Users/Melvrick/Desktop/Data Mining/drivers/1003")