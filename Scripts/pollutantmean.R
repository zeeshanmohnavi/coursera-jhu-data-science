pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either 'sulfate' or 'nitrate'
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  sum_pollutant <- 0
  total_nrow <- 0
  
  # Change the numeric id to include leading zeroes to align 
  # with the file names, e.g., 001.csv
  id_with_leading_zeros <- sprintf("%03d", id)
  
  for (file_id in id_with_leading_zeros) {
    file_path <- paste(directory, file_id, ".csv", sep = "")
    df <- read.csv(file_path)

    sum_pollutant = sum_pollutant + sum(df[, pollutant], na.rm = TRUE)
    total_nrow <- total_nrow + (nrow(df) - sum(is.na(df[, pollutant])))
  }
  sum_pollutant / total_nrow
}

# ## Test code block for pollutantmean()
# pollutantmean("./Data/specdata/", "sulfate", 1:10) # 4.064128
# pollutantmean("./Data/specdata/", "nitrate", 70:72) # 1.706047
# pollutantmean("./Data/specdata/", "nitrate", 23) # 1.280833
# 
# ## Quiz test cases
# pollutantmean("./Data/specdata/", "sulfate", 34) # 1.477
# pollutantmean("./Data/specdata/", "nitrate") # 1.703


# ## ---------------------------------------
# ## Code to check the performance of the function using system.time()
# # On a single sensor data: the baseline case
# # system.time(pollutantmean("./Data/specdata/", "nitrate", 1))
# 
# # On the complete set of data files
# # system.time(pollutantmean("./Data/specdata/", "nitrate", 1:332))
# ## ---------------------------------------
