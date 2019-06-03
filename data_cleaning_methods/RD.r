#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries

#METHOD NAME: Remove duplicates (RD)

#LIBRARIES:
  library(evaluate)

#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively
  
#Run the DNC method on the data first
  if(exists('master_DNC') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/DNC.R')))
  }
  
  master_RD <- master_DNC
  
#Function that identifies duplications in the data and prints the number out
#Duplications are defined as a data entry for the same individual that was entered on
#the same day
#Complete duplications are defined as a data entry for the same individual that was entered on
#the same day and is the same value (in this case, weight measurement)
  get_duplications <- function(X, print_results = TRUE) {
    X <- X %>%
      mutate(dups_ID = group_indices_(X, 
                                      .dots=c("ind_ID", "age_floored")), 
             complete_dups_ID = group_indices_(X, 
                                               .dots=c("ind_ID", "age_floored", "new_weight")),
             duplications = (duplicated(dups_ID) | 
                               duplicated(dups_ID, fromLast = TRUE)),
             complete_duplications = (duplicated(complete_dups_ID) |
                                        duplicated(complete_dups_ID, fromLast = TRUE)))
    if(print_results == TRUE) {
      print(c("Duplications", sum(X$duplications)))
      print(c("Complete Duplications", sum(X$complete_duplications)))
    }
    return(X)
  }

#check for any duplications
  master_RD <- get_duplications(master_RD)
  
#find the first and last observation in each group of duplicates
  master_RD<- master_RD %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup() %>%
    select(-c(last_obs_num, first_obs_num))

#Remove duplications by keeping the last (most recent) observation
  master_RD <- master_RD %>%
    filter(duplications == FALSE | duplications == TRUE & last_observation == TRUE)

#check for any remaining duplications
  master_RD <- get_duplications(master_RD)

