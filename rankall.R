# Load dplyr library, to enable the usage of the group_by_at and arrange APIs
library(dplyr)


################################################################################
### Name: rankall
### [In] outcome: A string indicates outcome type
### [In] num: Can be a numeric value to specify the rank of the required
###           hospital or a string to specify either the best or the worst
###           hopsital within a specified state.
### Return: A data frame with 2 columns, first column is the state name's
###         abbreviation and the second column is the hospital name which rank
###         matches the input "num" value for the corresponding state.
### Description: The function takes 2 arguments, an outcome and a rank. It
###              groups the hospitals per each state then sort them according
###              to the outcome rate and if the rate ties, it sort them based
###              on the alphabetical order. Later it creates a new data frame
###              with 2 columns, 1st is the state abbreviation and the 2nd is
###              the corresponding hospital name which rank matches the one
###              specified by the "num" input.
###              
###              
################################################################################
rankall <- function(outcome, num = "best") {
  # The index of the column holding the hospitals names
  col_name_idx <- 2
  # The index of the clumn holding the sates names
  col_state_idx <- 7
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
    # Create a list of all available states
    data_per_state <- split(file_cleaned, file_cleaned$State)
    
    # Define a function to sort each state's hospitals according to firstly,
    # the outcome rate, then secondly according to alphabetical order.
    sort_hospitals_per_state <- function(state) {
      # Create a reduced data frame with only three columns: hospital name,
      # state prefix and outcome rate
      red_data <- state[, c(col_name_idx, col_state_idx, target_outcome)]
      # Re-order according to the outcome rate column (last one in reduced df)
      red_data <- red_data[order(red_data[, 3]), ]
      # Group the reduced df by outcome rate, then arrange each group by
      # hospital name according to alphabetical order
      red_data <- red_data %>% group_by_at(outcome_col[[outcome]]) %>%
        arrange(Hospital.Name, .by_group = TRUE)
      return((red_data))
    }
    
    # Loop on all unique states and apply the sorting function for each state
    sorted_df_splitted <- lapply(data_per_state, sort_hospitals_per_state)
    
    # Create the final list data frame with only two columns: hospital name
    # and state abbreviation
    full_rank_list <- data.frame(
      hospital= character(),
      state= character()
    )
    
    # If num == "worst", loop on all states and fill the final list with the
    # the hospital name (column 1) that has the highest outcome rate (worst
    # hospital in the state) along with the state abbreviation (column 2).
    if ( num == "worst") {
      for (state in sorted_df_splitted) {
        full_rank_list[nrow(full_rank_list)+1, ] <-
          state[nrow(state), 1:2]
      }      
    }
    # If num == "best", loop on all states and fill the final list with the
    # the hospital name (column 1) that has the lowest outcome rate (best
    # hospital in the state) along with the state abbreviation (column 2).
    else if ( num == "best") {
      for (state in sorted_df_splitted) {
        full_rank_list[nrow(full_rank_list)+1, ] <-
          state[1, 1:2]
      }
    }
    # If num is a numeric value, then get the hospital name within each state,
    # that has the same rank as the num value. If the state has no rank matching
    # the num value, the returned row will contain NA for all columns, while it
    # is fine to have a hospital name with NA, the state's abbreviation will be
    # taken from the first row to make sure it will never be NA.
    else {
      for (state in sorted_df_splitted) {
        full_rank_list[nrow(full_rank_list)+1, ] <-
          c(state[num, 1], state[1,2])
      }      
    }

    
    return(full_rank_list)

  }
  else stop("invalid outcome")
}

new_data <- rankall("heart attack", 20)
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
