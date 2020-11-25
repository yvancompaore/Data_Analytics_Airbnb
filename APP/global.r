#read csv
read_data <- function(city){
  
  file_url <- file.path("data_cleansed", city)
  file_names <- list.files(file_url)
  files_paths <- c()
  
  for(i in 1:length(file_names)){
    date <- file_names[i]
    file_dir <- file.path("data_cleansed", city,date)
    file_subdirs <- list.dirs(file_dir)
    print(file_subdirs)
    files_paths <- c(files_paths, file_subdirs)

  }
  
  print(files_paths)
  files_paths <- file.path(files_paths, "listings.csv")
  listings <- 
    do.call(rbind,
            lapply(files_paths, read.csv, row.names=1))
}

data <- read_data("barcelona")