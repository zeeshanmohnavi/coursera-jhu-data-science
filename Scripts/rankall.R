## Return a data frame with the hospital names and the (abbreviated) 
## state name at the given rank (num) for the 30-day mortality rate
## for the given 'outcome'

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data_file_path <- "./Data/hospitaldata/outcome-of-care-measures.csv"
  outcome_data <- read_hospital_data(data_file_path)
  
  ## Check that outcome is valid
  check_outcome(outcome)
  
  ## For each state, find the hospital of the given rank
  
  ## Retrieve a list of the state names present in the data
  states <- unique(outcome_data[, 'State'])
  
  ## Create a data frame to store the result
  hospitals <- data.frame(hospital = character(), state = character(),
                          stringsAsFactors = FALSE)
  
  ## Create a valid variable name for the given 'outcome'
  outcome_col_name <- create_outcome_col_name(outcome_data, outcome)
  
  ## Create a data frame containing the hospital name, state and the
  ## mortality rate for the given outcome
  df <- outcome_data[, c('Hospital.Name', 'State', outcome_col_name)]
  
  ## Coerce the outcome variable to numeric for ease of processing
  df[, outcome_col_name] <- as.numeric(df[, outcome_col_name])
  
  ## Remove any observations with value 'NA'
  df <- df[complete.cases(df), ]

  ## Create an ordered data frame based on (state, mortality rate, 
  ## hospital name)
  ordered_hospital_data <- df[order(df[, 2], df[, 3], df[, 1]), ]

  ## Generate a numeric rank for the given rank (num)
  rank_index <- num
  if (num == 'best') {
    rank_index <- 1
  }

  for (state in states) {
    if (num == 'worst') {
      rank_index <- nrow(
        ordered_hospital_data[ordered_hospital_data$State == state, ])
    }
    
    ## Retrieve the hospital name from the given 'state' and rank
    hosp_name <- ordered_hospital_data[
      ordered_hospital_data$State == state, ][rank_index, 1]

    ## For debugging
    # print(paste(state, rank_index, hosp_name))

    ## Populates the result data frame with (hospital name, state) values
    ## [This is a workaround taken from https://stackoverflow.com/questions/5231540/r-losing-column-names-when-adding-rows-to-an-empty-data-frame]
    ## The first insertion to the data frame was overwriting the column
    ## names of the data frame, which was behaving unexpectedly for 
    ## subsequent insertions
    hospitals[nrow(hospitals) + 1, ] <- c(hosp_name, state)
  }
  
  ## Sort the resultant data frame on the state variable
  hospitals <- hospitals[order(hospitals$state), ]
  
  hospitals
}


## Test code block for rankall()

# Test case 1
# head(rankall("heart attack", 20), 10)
#                               hospital state
# 2                                 <NA>    AK
# 1       D W MCMILLAN MEMORIAL HOSPITAL    AL
# 4    ARKANSAS METHODIST MEDICAL CENTER    AR
# 3  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# 5                SHERMAN OAKS HOSPITAL    CA
# 6             SKY RIDGE MEDICAL CENTER    CO
# 7              MIDSTATE MEDICAL CENTER    CT
# 9                                 <NA>    DC
# 8                                 <NA>    DE
# 10      SOUTH FLORIDA BAPTIST HOSPITAL    FL

# Test case 2
# tail(rankall("pneumonia", "worst"), 3)
#                                      hospital state
# 52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# 51                     PLATEAU MEDICAL CENTER    WV
# 53           NORTH BIG HORN HOSPITAL DISTRICT    WY

# Test case 3
# tail(rankall("heart failure"), 10)
#                                                             hospital state
# 44                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# 45                                        FORT DUNCAN MEDICAL CENTER    TX
# 46 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# 49                                          SENTARA POTOMAC HOSPITAL    VA
# 48                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# 47                                              SPRINGFIELD HOSPITAL    VT
# 50                                         HARBORVIEW MEDICAL CENTER    WA
# 52                                    AURORA ST LUKES MEDICAL CENTER    WI
# 51                                         FAIRMONT GENERAL HOSPITAL    WV
# 53                                        CHEYENNE VA MEDICAL CENTER    WY


## Quiz test cases
#
# Test case 4
# r <- rankall("heart attack", 4)
# as.character(subset(r, state == "HI")$hospital)
# [1] "CASTLE MEDICAL CENTER"

# Test case 5
# r <- rankall("pneumonia", "worst")
# as.character(subset(r, state == "NJ")$hospital)
# [1] "BERGEN REGIONAL MEDICAL CENTER"

# Test case 6
# r <- rankall("heart failure", 10)
# as.character(subset(r, state == "NV")$hospital)
# [1] "RENOWN SOUTH MEADOWS MEDICAL CENTER"
