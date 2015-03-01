source("packages.R",echo = TRUE)
source("helpers.R", echo = TRUE)

current_directory = getwd()
parent_directory = dirname(current_directory)
drivers_directory = p(parent_directory,"/drivers") #see helpers.R for concat of strings

print(parent_directory)
print(drivers_directory)

list.dirs(drivers_directory,,,TRUE)

#files<-list.files(drivers_directory);dirname(getwd())
#for (i in files){
  print(files[i]);
  driver_files = list.files(drivers_directory);
  #inp <- read.csv(file=files[i], header=TRUE) 
  #name <- sub(".csv", "", files[i]) 
  #cat("Read ", files[i], "\trows: ", nrow(inp), " cols: ", ncol(inp),  "\n") 
  #eval(paste(name, "<- inp"))
#}
