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

# ## Test code block for corr()
# 
# # Test case 1
# # ---------------------------------------
# cr <- corr("./Data/specdata/", 150)
# head(cr)
# ## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
# summary(cr)
# ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# ## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313
# # ---------------------------------------
# 
# # Test case 2
# # ---------------------------------------
# cr <- corr("./Data/specdata/", 400)
# head(cr)
# ## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
# summary(cr)
# ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# ## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313
# # ---------------------------------------
# 
# # Test case 3
# # ---------------------------------------
# cr <- corr("./Data/specdata/", 5000)
# summary(cr)
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## 
# length(cr)
# ## [1] 0
# # ---------------------------------------
# 
# # Test case 4
# # ---------------------------------------
# cr <- corr("./Data/specdata/")
# summary(cr)
# ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# ## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000
# length(cr)
# ## [1] 323
# # ---------------------------------------


# ## Quiz test cases
# # Test case 5
# # ---------------------------------------
# cr <- corr("./Data/specdata/")                
# cr <- sort(cr)   
# RNGversion("3.5.1")
# set.seed(868)                
# out <- round(cr[sample(length(cr), 5)], 4)
# print(out)
# # [1]  0.2688  0.1127 -0.0085  0.4586  0.0447
# # ---------------------------------------

# # Test case 6
# # ---------------------------------------
# cr <- corr("./Data/specdata/", 129)                
# cr <- sort(cr)                
# n <- length(cr)    
# RNGversion("3.5.1")
# set.seed(197)                
# out <- c(n, round(cr[sample(n, 5)], 4))
# print(out)
# # [1] 243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969
# # ---------------------------------------


# # Test case 7
# # ---------------------------------------
# cr <- corr("./Data/specdata/", 2000)                
# n <- length(cr)                
# cr <- corr("./Data/specdata/", 1000)                
# cr <- sort(cr)
# print(c(n, round(cr, 4)))
# # [1]  0.0000 -0.0190  0.0419  0.1901
# # ---------------------------------------


# ## ---------------------------------------
# ## Code to check the performance of the function using system.time()
# # On a single sensor data: the baseline case
# # system.time(cr <- corr("./Data/specdata/", threshold = 1) )
# 
# # On the complete set of data files
# # system.time(cr <- corr("./Data/specdata/", threshold = 1090) )
# ## ---------------------------------------
