best <- function(state,outcome) {
  ## Read outcome data

  library(plyr)
  library(tidyr)
  
  #drop unneeded columns
  
  outcome_df <- as.tibble(read.table("outcome-of-care-measures.csv", header=TRUE, sep = ",",
                         colClasses = c("NULL", "character", "NULL", "NULL", "NULL", "NULL", "character", "NULL","NULL","NULL",
                                        "character", "NULL","NULL","NULL","NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character",
                                        "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL")))



#print(head(outcome_df))

## Check that state and outcome are valid
  
  #check that state entered in 'best' function is valid  
   
  state1 <- state
  state_valid_abb <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','PR','RI','SC','SD','TN','TX','UT','VT','VI','VA','WA','WV','WI','WY','GU')
  #print(state1)
  if (state1 %in% state_valid_abb) {
    #print("success")
    } else {
      print("invalid state")
    }
 
  outcome1 <- outcome
#check that outcome entered in 'best' function is valid
  outcome_valid_choice <- c('heart attack', 'heart failure', 'pneumonia')
  if (outcome %in% outcome_valid_choice) {
    #print("valid outcome entered") 
      } else {
        print("invalid outcome")
      }
  
print(names(outcome_df))
## Return hospital name in that state with lowest 30-day death
## rate

names(outcome_df)[3] <- 'heart_attack_mortality'
names(outcome_df)[4] <- 'heart_failure_mortality'
names(outcome_df)[5] <- 'pneumonia_mortality'

#change data types for 3 columns from character to numeric
outcome_df$heart_attack_mortality <- as.numeric(outcome_df$heart_attack_mortality)
outcome_df$heart_failure_mortality <- as.numeric(outcome_df$heart_failure_mortality)
outcome_df$pneumonia_mortality <- as.numeric(outcome_df$pneumonia_mortality)

print(head(outcome_df))

outcome_df2 = ''

  if (outcome == 'heart attack'){
    #eliminate bad rows for heart attack data
    outcome_df2<- outcome_df[!is.na(outcome_df$heart_attack_mortality),]
    outcome_df3 <- outcome_df2[order(heart_attack_mortality),] 
    }
  
  if (outcome == 'heart failure'){
    #eliminate bad rows for heart failure data
    
    outcome_df2 <- outcome_df[order(heart_failure_mortality),] 
    }
  
  if (outcome == 'pneumonia') {
    #eliminate bad rows for pneumonia data
    
    outcome_df2 <- outcome_df[order(pneumonia_mortality),] 
    }
 
print(outcome_df2)
print(outcome_df2[1,1])
  
