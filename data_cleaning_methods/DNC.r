#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a 'null' method of data cleaning: not making
#any changes or modifications to the data

#METHOD NAME: DO NOT CLEAN (DNC)

#LIBRARIES:
  library(tidyverse)
  library(evaluate)
  
#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively

#read in data

  if(exists('dat') == FALSE) {
<<<<<<< HEAD
    dat <- read_csv('../data/simulated_CLOSER_data.csv')
  }
    
  #get duplications is a Function that identifies duplications in the data and prints the number out
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
dat <- get_duplications(dat)
  
=======
    replay(evaluate(file('../data_prep_and_error_simulation/simulate_errors_CLOSER_data.R')))
  }
  
#run the setup file for all data cleaning methods
  replay(evaluate(file('../data_cleaning_methods/setup_file.R')))
    
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
#Save cleaned data
  master_DNC <- dat


