complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Change the numeric id to include leading zeroes to align 
  # with the file names, e.g., 001.csv
  id_with_leading_zeroes <- sprintf("%03d", id)
  
  # Create an empty data frame with two columns, id and nobs
  df_complete <- data.frame(id = numeric(), nobs = numeric())
  column_names <- colnames(df_complete)

  for (file_id in id_with_leading_zeroes) {
    file_path <- paste(directory, file_id, ".csv", sep = "")
    df <- read.csv(file_path)

    # Subset the data frame to include only the rows with
    # no column value as NA
    df <- df[complete.cases(df), ]

    df_complete <- rbind.data.frame(df_complete, c(as.numeric(file_id), nrow(df)))
  }
  colnames(df_complete) <- column_names
  df_complete
}

## Test code block for complete()

complete("./Data/specdata/", 1)
##   id nobs
## 1  1  117

complete("./Data/specdata/", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

complete("./Data/specdata/", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

complete("./Data/specdata/", 3)
##   id nobs
## 1  3  243
