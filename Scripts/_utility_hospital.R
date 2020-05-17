## Utility module for Programming Assignment 3 on US Hospital Data
## This module contain four (4) functions: read_hospital_data,
## check_state, check_outcome, and create_outcome_col_name


## Read in a CSV file from the given 'source_file_path'
## Return a data frame containing the hospital data. All columns
## of the data frame are explicitly set as 'character'

read_hospital_data <- function(source_file_path) {
  outcome_data <- read.csv(source_file_path, colClasses = "character")
  outcome_data
}


## Read in a data frame and the name of a state in the US
## Check whether there are any observations in the data frame
## where the variable 'State' have a value of 'state'. If not,
## stop the execution and display an appropriate message.

check_state <- function(outcome_data, state) {
  if (!state %in% outcome_data[, 'State']) {
    stop("invalid state")
  }
}


## Read in an argument 'outcome' which represents an outcome
## of patients in the Hospital Dataset.
## Check the validity of the outcome. If the value of 'outcome'
## is not valid, stop the execution and display an appropriate
## message.

check_outcome <- function(outcome) {
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% possible_outcomes) {
    stop("invalid outcome")
  }
}


## Create the column name 'col_name' as present in the data frame to 
## help select the appropriate Death Mortality Rate for the given 
## 'outcome'
## Arguments:
## outcome_data    the complete data frame
## outcome    the name of the outcome
## Return Value: the column name for the provided 'outcome' argument

create_outcome_col_name <- function(outcome_data, outcome) {
  if (outcome == 'heart attack') {
    col_index <- 11
  } else if (outcome == 'heart failure') {
    col_index <- 17
  } else if (outcome == 'pneumonia') {
    col_index <- 23
  }
  col_name <- names(outcome_data)[col_index]
  col_name
}
