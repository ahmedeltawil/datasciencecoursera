
best <- function(state, outcome) {
  ## Read outcome
  col_name_idx <- 2
  outcome_l <- list("heart attack" = 11, "heart failure"= 17, "pneumonia"= 23)
  file <- 
    read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", 
             colClasses = "character")
  if(outcome %in% names(outcome_l)) {
    target_outcome <- outcome_l[[outcome]]
    suppressWarnings(file[, target_outcome] 
                     <- as.numeric(file[, target_outcome]))
  
    file_cleaned <- file[complete.cases(file), ]  

    outcome_rate <- 
      file_cleaned[file_cleaned$State== state, c(col_name_idx, target_outcome)]

        if (nrow(outcome_rate) == 0) stop("invalid state")
    else {
            outcome_rate <- 
              outcome_rate[outcome_rate[2] ==
                             min(outcome_rate[2]), ]

            winner_list <- outcome_rate$Hospital.Name
            winner_list <- sort(winner_list)
            return(winner_list[1])

    }
  }
  else stop("invalid outcome")
}

# Examples:

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
