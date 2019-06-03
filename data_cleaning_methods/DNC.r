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
    replay(evaluate(file('../data_prep_and_error_simulation/simulate_errors_CLOSER_data.R')))
  }
  
#run the setup file for all data cleaning methods
  replay(evaluate(file('../data_cleaning_methods/setup_file.R')))
    
#Save cleaned data
  master_DNC <- dat


