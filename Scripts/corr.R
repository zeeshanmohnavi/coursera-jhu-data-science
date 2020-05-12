corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  df_complete <- complete(directory)
  df_greater_than_threshold <- subset(df_complete, df_complete$nobs > threshold)
  
  correlations <- vector(mode = "numeric")
  
  for(file_id in unlist(df_greater_than_threshold['id'])) {
    file_path <- paste(directory, sprintf("%03d", file_id), ".csv", sep = "")
    df <- read.csv(file_path)
    df <- df[complete.cases(df), ]

    sulfate <- df[, 'sulfate']
    nitrate <- df[, 'nitrate']
    correlations <- append(correlations, cor(sulfate, nitrate))
  }
  correlations
}

