#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an internally defined cut-off (longitudinal z-score) to remove outliers

#METHOD NAME: Longitudinal z-score cut-off (LZCO)

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

  LZCO <- master_DNC
 
=======
#Run the RD method on the data first
  if(exists('master_RD') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/RD.R')))
  }

  master_LZCO <- master_RD

>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
#For the LZCO method, data is divided into 365 day age periods by sex and the z scores 
#are calculated with each age/sex group
#Function that will get appropriate intervals according to the data
  get_intervals <- function(data, int, Freq_lim) {
    X <- as.data.frame(table(cut(data$age, breaks = intervals, include.lowest = TRUE,  dig.lab = 5))) %>%
      mutate(ID = 1:nrow(.),
             empty_capacity = max(which(Freq < Freq_lim)),
             empty_capacity2 = (ID == empty_capacity),
             empty_capacity3 = case_when(ID == 2 & ((Freq[ID == 1] < Freq_lim) == TRUE) ~ TRUE,
                                         ID == 2 & ((Freq[ID == 1] < Freq_lim) == FALSE) ~ empty_capacity2,
                                         ID != 2 ~ empty_capacity2))
    success <- FALSE
    while (!success) {
      X <- subset(X, X$empty_capacity3 == FALSE) %>%
        mutate(bins2 = gsub("\\[|\\]|\\(|\\)", "", Var1)) %>%
        separate(bins2, c("lower_bin", "upper_bin"), ",") 
      intervals <- sort(unique(as.numeric(c(min(int), as.numeric(X$lower_bin), max(int)))))
      X <- as.data.frame(table(cut(data$age, breaks = intervals, include.lowest = TRUE,  dig.lab = 5))) %>%
        mutate(ID = 1:nrow(.),
               empty_capacity = max(which(Freq < Freq_lim)),
               empty_capacity2 = (ID == empty_capacity),
               empty_capacity3 = case_when(ID == 2 & ((Freq[ID == 1] < Freq_lim) == TRUE) ~ TRUE,
                                           ID == 2 & ((Freq[ID == 1] < Freq_lim) == FALSE) ~ empty_capacity2,
                                           ID != 2 ~ empty_capacity2))
      success <- any(X$Freq < Freq_lim) == FALSE
    }
    X <- subset(X, X$empty_capacity3 == FALSE) %>%
      mutate(bins2 = gsub("\\[|\\]|\\(|\\)", "", Var1)) %>%
      separate(bins2, c("lower_bin", "upper_bin"), ",") 
    return(X)
  }

<<<<<<< HEAD
  #Set initial intervals with the ideal time period required and run the function
  intervals <- seq(min(dat$age), max(dat$age+age_interval_cutoff), age_interval_cutoff)
  bin_freq <- get_intervals(dat, intervals, bin_freq_cutoff)
  
  #get the appropriate intervals from the new data
  intervals <- sort(unique(as.numeric(c(min(intervals), as.numeric(bin_freq$lower_bin), max(intervals)))))
  
  LZCO <- LZCO %>%
    mutate(bins = cut(LZCO$age, breaks = intervals, include.lowest = TRUE))

#define the z-scores for weight (by each group) that are used to identify outliers
#The cut off for z-score is given as more than 3 or less than -3
  LZCO <- as.tbl(LZCO) 
  
  LZCO <- z_score_cutoffs2(LZCO)
  
  #check for any duplications
  LZCO <- get_duplications(LZCO)
  
  #find the first and last observation in each group of duplicates
  LZCO <- LZCO %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup() %>%
    select(-c(last_obs_num, first_obs_num))
  
  #Remove duplications by keeping the last (most recent) observation
  LZCO <- LZCO %>%
    filter(duplications == FALSE | duplications == TRUE & last_observation == TRUE)
  
  #check for any remaining duplications
  LZCO <- get_duplications(LZCO)

#cut out outliers to clean the data
  master_LZCO <- LZCO %>%
    filter(z_outlier == FALSE)

  
  
    
=======
#Set initial intervals with the ideal time period required and run the function
    intervals <- seq(min(dat$age), max(dat$age+age_interval_cutoff), age_interval_cutoff)
    bin_freq <- get_intervals(dat, intervals, bin_freq_cutoff)

#get the appropriate intervals from the new data
  intervals <- sort(unique(as.numeric(c(min(intervals), as.numeric(bin_freq$lower_bin), max(intervals)))))

  master_LZCO <- master_LZCO %>%
    mutate(bins = cut(master_LZCO$age, breaks = intervals, include.lowest = TRUE))

#define the z-scores for weight (by each group) that are used to identify outliers
#The cut off for z-score is given as more than 3 or less than -3
  master_LZCO <- as.tbl(master_LZCO) %>%
    group_by(bins, sex) %>%
    mutate(pop_mean_long = mean(weight),
           pop_sd_long = sd(weight),
           z_score_long = abs((weight - pop_mean_long) / pop_sd_long),
           z_score_outlier_long = z_score_long > z_score_cutoff) %>%
    ungroup()
  
#cut out outliers to clean the data
  master_LZCO <- master_LZCO %>%
    filter(z_score_outlier_long == FALSE)
  
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
