rankall <- function(outcome, num = "best") {

    # The function reads the outcome-of-care-measures.csv file #
    #install.packages("tibble")
    #library(plyr)
    #library(dplyr)
    #library(tidyr)
    #options(warn=-1)
    #drop unneeded columns
  
    solution_df <-data.frame()
  
    outcome_df <- as.tibble(read.table("outcome-of-care-measures.csv", header=TRUE, sep = ",",
                                       colClasses = c("NULL", "character", "NULL", "NULL", "NULL", "NULL", "character", "NULL","NULL","NULL",
                                                      "character", "NULL","NULL","NULL","NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character",
                                                      "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL")))
    
    state_valid_abb <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','PR','RI','SC','SD','TN','TX','UT','VT','VI','VA','WA','WV','WI','WY','GU')
  
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
      outcome_df3 <- arrange(outcome_df2, heart_attack_mortality, Hospital.Name) #sorted, all states
    }
    
    if (outcome == 'heart failure'){
      #eliminate bad rows for heart failure data and sort ascending
      
      outcome_df2<- outcome_df[!is.na(outcome_df$heart_failure_mortality),]
      outcome_df3 <- arrange(outcome_df2, heart_failure_mortality, Hospital.Name) #sorted, all states
    }
    
    if (outcome == 'pneumonia') {
      #eliminate bad rows for pneumonia data and sort ascending
      
      outcome_df2<- outcome_df[!is.na(outcome_df$pneumonia_mortality),]
      outcome_df3 <- arrange(outcome_df2, pneumonia_mortality, Hospital.Name) #sorted, all states
    }
    
    
    
    for(state in state_valid_abb) {
     
      #limit by state
      outcome_df4 <- subset(outcome_df3, State == state) #vectorized subset produces df with one state only

      length_sorted <- nrow(outcome_df4) #count number of rows in df limited by state
      
      if(num == "worst") {
        rank <- length_sorted }
      
      if (rank > length_sorted) {
        solution <- paste("NA",state)
      } else {
        solution <- paste(c(pull(outcome_df4[rank, 1]),state))
      }
      solution_df <- rbind(solution_df, solution)
    }
      return(solution_df)

} # END FUNCTION
