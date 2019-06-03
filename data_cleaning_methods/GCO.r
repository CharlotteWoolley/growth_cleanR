#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an externally defined cut-off to remove outliers

#METHOD NAME: General cut-off (GCO)

#LIBRARIES:
  library(evaluate)

#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively
  
#Run the RD method on the data first
  if(exists('master_RD') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/RD.R')))
  }

  master_GCO <- master_RD

#define the externally defined cut-offs for weight that are used to identify outliers
  master_GCO <- general_cutoffs(master_GCO)
  
#remove values defined as outliers  
  master_GCO <- master_GCO %>%
    filter(cut_outlier == FALSE)

