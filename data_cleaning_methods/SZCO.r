#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an internally defined cut-off (cross-sectional z-score) to remove outliers

#METHOD NAME: Standard z-score cut-off (SZCO)

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
  
  #define the z-scores for weight that are used to identify outliers
  #The cut off for z-score is given as more than 3 or less than -3
  SZCO <- z_score_cutoffs(master_DNC)
  
  #check for any duplications
  SZCO <- get_duplications(SZCO)
  
  #find the first and last observation in each group of duplicates
  SZCO <- SZCO %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup() %>%
    select(-c(last_obs_num, first_obs_num))
  
  #Remove duplications by keeping the last (most recent) observation
  SZCO <- SZCO %>%
    filter(duplications == FALSE | duplications == TRUE & last_observation == TRUE)
  
  #check for any remaining duplications
  SZCO <- get_duplications(SZCO)

  #remove values defined as outliers  
  master_SZCO <- SZCO %>%
    filter(z_outlier == FALSE)
  
=======
#Run the RD method on the data first
  if(exists('master_RD') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/RD.R')))
  }

  SZCO <- master_RD

#define the z-scores for weight that are used to identify outliers
#The cut off for z-score is given as more than 3 or less than -3
  SZCO <- SZCO %>% 
    mutate(pop_mean = mean(weight),
           pop_sd = sd(weight),
           z_score = abs((weight - pop_mean) / pop_sd),
           z_score_outlier = z_score > z_score_cutoff)
  
#cut out outliers to clean the data
  master_SZCO <- SZCO %>%
    filter(z_score_outlier == FALSE)

>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
