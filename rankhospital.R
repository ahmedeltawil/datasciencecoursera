# Load dplyr library, to enable the usage of the group_by_at and arrange APIs
library(dplyr)

################################################################################
### Name: rankhospital
### [In] state: Two characters indicate state's name
### [In] outcome: A string indicates outcome type
### [In] num: Can be a numeric value to specify the rank of the required
###           hospital or a string to specify either the best or the worst
###           hopsital in the specified state.
### Return: The hospital name which implies the input rank within the input
###         state.
### Description: The function takes a state code, outcome type and a specific
###              ranking. Later it reads and store the database for different
###              hospitals across many states, sort hospitals according to the
###              given outcome and if two hospitals share the same ranking, the
###              tie should be resolved in alphabetic order. Finally it should
###              return to the user the hospital name according to the input
###              ranking. If the ranking (num) is set to "best" the lowest rate
###              should be returned while the highest is returned if it is
###              set to "worst"
################################################################################
rankhospital <- function(state, outcome, num = "best") {
  # The index of the column holding the hospitals names
  col_name_idx <- 2
  # A list of column indices correspond to the outcome rates.
  outcome_idx <- list("heart attack" = 11, "heart failure"= 17, "pneumonia"= 23)
  # A list of column names correspond to the outcome rates.
  outcome_col <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                      "heart failure"= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      "pneumonia"    = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") 
  # load the outcome data base into a dataframe
  file <- 
    read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", 
             colClasses = "character")
  
  # Check if the input outcome really exist in our data set.
  if(outcome %in% names(outcome_idx)) {
    
    # Get the column index correspond to the input outcome
    target_outcome <- outcome_idx[[outcome]]
    # Convert the whole outcome column to numeric class
    suppressWarnings(file[, target_outcome] 
                     <- as.numeric(file[, target_outcome]))
    # Remove NAs from the data frame
    file_cleaned <- file[complete.cases(file), ]  
    
    # Extract only rows related to the input state into a subset data frame,
    # contains only two columns: hopital name and outcome rate.
    outcome_rate <- 
      file_cleaned[file_cleaned$State== state, c(col_name_idx, target_outcome)]
    
    # Check if the state subset dataframe is empty, then execution should stop
    if (nrow(outcome_rate) == 0) stop("invalid state")
    else {
      # Re-arrange the state data frame according to the outcome failure rate
      # in ascending order
      outcome_rate <- outcome_rate[order(outcome_rate[,2]), ]
      # For each group of hospital sharing the same failure rate, a re-arrange
      # to the data frame rows is applied according to the alphabetical order
      # of hospitals names
      outcome_sorted <- outcome_rate %>% group_by_at(outcome_col[[outcome]]) %>%
        arrange(Hospital.Name, .by_group = TRUE)
      # If ranking input is a string of value "best", return the hospital name
      # in the first row (lowest failure rate)
      if(num == "best")
        return (outcome_sorted[1,1])
      # If ranking input is a string of value "worst", return the hospital name
      # in the first row (highest failure rate)
      else if(num == "worst")
        return (outcome_sorted[nrow(outcome_sorted), 1])
      else
        # Return [NA] if the rank required, exceed the maximum ranking within
        # the given state
        if (num > nrow(outcome_sorted))
          return (NA)
        # If none of the previous conditions were applied for the "num" input
        # then the usual behavior is to return the row that match the input
        # ranking.
        else
          return (outcome_sorted[num, 1])
    }
  }
  else stop("invalid outcome")
}


rankhospital("TX", "heart failure", 4)
result_arranged <- result %>%
  group_by(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
  arrange(Hospital.Name, .by_group = TRUE)
rankhospital("TX", "heart failure", "best")
rankhospital("TX", "heart failure", 285)
rankhospital("TX", "heart failure", "worst")

rankhospital("MD", "heart attack", "worst")
rankhospital("MD", "heart attack", "best")
rankhospital("MN", "heart attack", 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
