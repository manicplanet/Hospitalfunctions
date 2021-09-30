rankhospital <-function(state, outcome, num = "best") {

# The function reads the outcome-of-care-measures.csv file #
  #install.packages("tibble")
  #library(plyr)
  #library(dplyr)
  #library(tidyr)
  #options(warn=-1)
  #drop unneeded columns

  outcome_df <- as.tibble(read.table("outcome-of-care-measures.csv", header=TRUE, sep = ",",
                                   colClasses = c("NULL", "character", "NULL", "NULL", "NULL", "NULL", "character", "NULL","NULL","NULL",
                                                  "character", "NULL","NULL","NULL","NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character",
                                                  "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL")))



  ## Check that state and outcome are valid

  state_valid_abb <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','PR','RI','SC','SD','TN','TX','UT','VT','VI','VA','WA','WV','WI','WY','GU')
  #print(state1)
  if (state %in% state_valid_abb) {
    #print("success")
  } else {
    print("invalid state")
    exit()
  }

  
  
  
  if (num == "best"){
    rank <- 1
  }

  if (num =="worst") {
    rank <- 10000
  }
  
  if (num !="worst && num != 'best") {
    rank <- num
  }    
  
  
  
  outcome1 <- outcome
  #check that outcome entered in 'best' function is valid
  outcome_valid_choice <- c('heart attack', 'heart failure', 'pneumonia')
  if (outcome %in% outcome_valid_choice) {
    #print("valid outcome entered")
  } else {
    print("invalid outcome")
    exit()
  }

names(outcome_df)[3] <- 'heart_attack_mortality'
names(outcome_df)[4] <- 'heart_failure_mortality'
names(outcome_df)[5] <- 'pneumonia_mortality'

#change data types for 3 columns from character to numeric
outcome_df$heart_attack_mortality <- as.numeric(outcome_df$heart_attack_mortality)
outcome_df$heart_failure_mortality <- as.numeric(outcome_df$heart_failure_mortality)
outcome_df$pneumonia_mortality <- as.numeric(outcome_df$pneumonia_mortality)

#print(head(outcome_df))

outcome_df2 = ''

if (outcome == 'heart attack'){
  #eliminate bad rows for heart attack data and sort ascending
  outcome_df2<- outcome_df[!is.na(outcome_df$heart_attack_mortality),]
  outcome_df3 <- arrange(outcome_df2, heart_attack_mortality, Hospital.Name)
}

if (outcome == 'heart failure'){
  #eliminate bad rows for heart failure data and sort ascending

  outcome_df2<- outcome_df[!is.na(outcome_df$heart_failure_mortality),]
  outcome_df3 <- arrange(outcome_df2, heart_failure_mortality, Hospital.Name)
}

if (outcome == 'pneumonia') {
  #eliminate bad rows for pneumonia data and sort ascending

  outcome_df2<- outcome_df[!is.na(outcome_df$pneumonia_mortality),]
  outcome_df3 <- arrange(outcome_df2, pneumonia_mortality, Hospital.Name)
}

#limit by state
outcome_df3 <- subset(outcome_df3, State == state)


length_sorted <- nrow(outcome_df3)


if (rank > length_sorted) {
  print("Rank input is higher than number of institutions in state.")
  return(NA)
  exit()
}

solution <- pull(outcome_df3[rank, 1])
return(solution)


}
