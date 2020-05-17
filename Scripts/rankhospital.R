## Return hospital name in the given 'state' with the given rank
## for the given 30-day death mortality rate for 'outcome'

rankhospital <- function(state, outcome, num = 'best') {
  ## Read outcome data
  data_file_path <- "./Data/hospitaldata/outcome-of-care-measures.csv"
  outcome_data <- read_hospital_data(data_file_path)
  
  ## Check that state and outcome are valid
  check_state(outcome_data, state)
  check_outcome(outcome)
  
  ## Create a valid variable name for the given 'outcome'
  outcome_col_name <- create_outcome_col_name(outcome_data, outcome)
  
  ## Create a data frame containing the hospital name and the
  ## mortality rate for the given state and outcome
  state_data <- subset(outcome_data, outcome_data$State == state,
                       select = c('Hospital.Name', outcome_col_name))
  
  ## Coerce the outcome variable to numeric for ease of processing
  state_data[outcome_col_name] <- as.numeric(state_data[, outcome_col_name])
  
  ## Sort the data frame based on the mortality rate. Use the hospital 
  ## name to break ties
  sorted_state_data <- state_data[order(state_data[outcome_col_name],
                                        state_data['Hospital.Name']), ]
  
  ## Generate a numeric rank for the given rank (num)
  rank_index <- num
  if (num == 'best') {
    rank_index <- 1
  }
  if (num == 'worst') {
    rank_index <- nrow(sorted_state_data[complete.cases(sorted_state_data), ])
  }
  
  ## Retrieve the hospital name ranked at 'rank_index' for the mortality
  ## rate for the given outcome
  sorted_state_data[[1]][rank_index]
}


# ## Test code block for rankhospital()
#  
# # Test case 1
# # rankhospital("TX", "heart failure", 4)
# # [1] "DETAR HOSPITAL NAVARRO"
# 
# # Test case 2
# # rankhospital("MD", "heart attack", "worst")
# # [1] "HARFORD MEMORIAL HOSPITAL"
# 
# # Test case 3
# # rankhospital("MN", "heart attack", 5000)
# # [1] NA
# 
# ## Quiz test cases
# #
# # Test case 4
# # rankhospital("NC", "heart attack", "worst")
# # 1] "WAYNE MEMORIAL HOSPITAL"
# 
# # Test case 5
# # rankhospital("WA", "heart attack", 7)
# # [1] "YAKIMA VALLEY MEMORIAL HOSPITAL"
# 
# # Test case 6
# # rankhospital("TX", "pneumonia", 10)
# # [1] "SETON SMITHVILLE REGIONAL HOSPITAL"
# 
# # Test case 7
# # rankhospital("NY", "heart attack", 7)
# # [1] "BELLEVUE HOSPITAL CENTER"
