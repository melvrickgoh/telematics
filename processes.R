retrieve.driver_trip_distributions <- function(directory_locale,calc_type){
  trip_frame <- read.csv(file_locale, header=TRUE)
  
  switch(calc_type,"speed_dist"={
    return(speedDistribution(trip_frame,1))
  },"tangent_a_dist"={
    return(TangAccelDistribution(trip_frame,1))
  },"normal_a_dist"={
    return(NormAccelDistribution(trip_frame,1))
  },"total_a_dist"={
    return(TotalAccelDistribution(trip_frame,1))
  })
}

retrieve.driver_trip_raw_calculation <- function(directory_locale,calc_type){
  trip_frame <- read.csv(file_locale, header=TRUE)
  
  switch(calc_type,"distance"={
    return(distance(trip_frame,1))
  },"speed"={
    return(calcSpeed(trip_frame,1))
  },"tangent_acceleration"={
    return(calcTangAccel(trip_frame,1))
  },"normal_acceleration"={
    return(calcNormAccel(trip_frame,1))
  },"curvature"={
    return(calcCurvature(trip_frame,1))
  })
}
#iterate through all individual driver folders
proc.all_drivers <- function(parent_directory = "."){
  driver_dir_locales = list.files(parent_directory,,,TRUE)
  
  for (locale in driver_dir_locales){
    proc.driver_trips(locale)
  }
}

proc.selective_driver_trips <- function(parent_directory = ".",target_file_name){
  target_frames = list.read_selective_folders(target_file_name)
  for (targets in target_frames) {
    target_locale = p(parent_directory,"/",folder_name)
    print(target_locale)
  }
}

proc.all_driver_folder_names <- function(parent_directory = "."){
  driver_dir_locales = list.files(parent_directory,,,TRUE)
  
  for (locale in driver_dir_locales){
    print(basename(locale))
  }
}

#iterate through all individual driver trips
proc.driver_trips <- function(driver_directory = "."){
  driver_no = basename(driver_directory)
  driver_trip_files = list.files(driver_directory)
  print(driver_trip_files)
  names <- data.frame()
  for (file in driver_trip_files) {
    file_locale = p(driver_directory,"/",file) #file full location
    
    trip_file_name = basename(file_locale)
    
    trip_frame <- read.csv(file_locale, header=TRUE) 
    if(toofast(trip_frame,1)){
      print("Too fast");
      print(file)
      # print(toofast(trip_frame,1))  
      # v<-FALSE
    } else {
      #perform measures
      trip_feature_set <- tripFeatures(driver_no,trip_frame,trip_file_name,1)
      
      trip_summary_stats_frame <- data.frame(as.list(trip_feature_set))
      test_frame <- data.frame(lapply(trip_feature_set, type.convert), stringsAsFactors=FALSE)
      
      write.table(trip_summary_stats_frame, file = 'summary_stats.csv', append=TRUE, sep=",", row.names=FALSE,col.names=FALSE)
      
      #name <- sub(".csv", "", file_locale) 
      #cat("Read ", file_locale, "\trows: ", nrow(trip_frame), " cols: ", ncol(trip_frame),  "\n") 
      #eval(paste(name, "<- trip_frame"))
    }
  }
  print(p(driver_no," done"))
}