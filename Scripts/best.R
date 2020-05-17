## Return the hospital name in the given 'state' with the lowest
## 30-day mortality rate for the given 'outcome'

best <- function(state, outcome) {
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
  
  ## Retrieve the hospital name with the minimum mortality rate
  hospital_name_min_mortality <- sorted_state_data[[1]][1]
  
  hospital_name_min_mortality
}


# ## Test code block for best()
# 
# # Test case 1
# # best("TX", "heart attack")
# # [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# 
# # Test case 2
# # best("TX", "heart failure")
# # [1] "FORT DUNCAN MEDICAL CENTER"
# 
# # Test case 3
# # best("MD", "heart attack")
# #  "JOHNS HOPKINS HOSPITAL, THE"
# 
# # Test case 4
# # best("MD", "pneumonia")
# # "GREATER BALTIMORE MEDICAL CENTER"
# 
# # Test case 5
# # best("BB", "heart attack")
# # Error in best("BB", "heart attack") : invalid state
# 
# # Test case 6
# # best("NY", "hert attack")
# # Error in best("NY", "hert attack") : invalid outcome
# 
# ## Quiz test cases
#  
# # Test case 7
# # best("SC", "heart attack")
# # [1] "MUSC MEDICAL CENTER"
# 
# # Test case 8
# # best("NY", "pneumonia")
# # [1] "MAIMONIDES MEDICAL CENTER"
# 
# # Test case 9
# # best("AK", "pneumonia")
# # [1] "YUKON KUSKOKWIM DELTA REG HOSPITAL"
