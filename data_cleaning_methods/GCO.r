#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an externally defined cut-off to remove outliers

#METHOD NAME: General cut-off (GCO)

#LIBRARIES:
  library(evaluate)

#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively
  
<<<<<<< HEAD
  #Run the DNC method on the data first
  if(exists('master_DNC') == FALSE) {
    replay(evaluate(file('../data_cleaning_methods/DNC.R')))
  }
  
  #define the externally defined cut-offs for weight that are used to identify outliers
  GCO <- general_cutoffs(master_DNC)
  
  #check for any duplications
  GCO <- get_duplications(GCO)
  
  #find the first and last observation in each group of duplicates
  GCO <- GCO %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup() %>%
    select(-c(last_obs_num, first_obs_num))
  
  #Remove duplications by keeping the last (most recent) observation
  GCO <- GCO %>%
    filter(duplications == FALSE | duplications == TRUE & last_observation == TRUE)
  
  #check for any remaining duplications
  GCO <- get_duplications(GCO)

#remove values defined as outliers  
  master_GCO <- GCO %>%
=======
#Run the RD method on the data first
  if(exists('master_RD') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/RD.R')))
  }

  master_GCO <- master_RD

#define the externally defined cut-offs for weight that are used to identify outliers
  master_GCO <- general_cutoffs(master_GCO)
  
#remove values defined as outliers  
  master_GCO <- master_GCO %>%
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    filter(cut_outlier == FALSE)

