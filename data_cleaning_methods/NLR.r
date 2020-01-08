#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using a non-linear regression model for growth to identify outliers

#METHOD NAME: Non-linear regression model (NLR)

#LIBRARIES:
  library(evaluate)

#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively

<<<<<<< HEAD
  
  #MAKING AN APPROPRIATE NLR MODEL
  
  #Run the GCO method on the data first, to obtain a cleaner subset of the data
  #on which to run the model
  if(exists('master_GCO') == FALSE) {
    replay(evaluate(file('../data_cleaning_methods/GCO.R')))
  }
  
  subs <- master_GCO %>%
    mutate(ind_ID = as.factor(ind_ID),
           sex = as.factor(sex))
  
  #using the starting values, fit non-linear regression model to each sex
  #of the subset of data (with duplicates and general cut off outliers removed)
  
  females <- subset(subs, subs$sex == 'Female')
=======
#Run the GCO method on the data first, to obtain a cleaner subset of the data
#on which to run the model
  if(exists('master_GCO') == FALSE) {
    replay(evaluate(file('../data_cleaning_methods/GCO.R')))
  }

  sub_dat <- master_GCO
  NLR <- master_DNC

#using these starting values, fit non-linear regression model to each sex
#of the subset of data (with duplicates and general cut off outliers removed)

  females <- subset(sub_dat, sub_dat$sex == 'Female')
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  fem_mod <- nls(f1,
                 data = females,
                 start = c(Asym = fem_start[[1]], lag_time = fem_start[[2]],
                           growth_rate = fem_start[[3]]))
  
  fem_mod_pars <- fem_mod$m$getPars()  
<<<<<<< HEAD
  
  males <- subset(subs, subs$sex == 'Male')
=======
    
  males <- subset(sub_dat, sub_dat$sex == 'Male')
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  mal_mod <- nls(f1,
                 data = males,
                 start = c(Asym = mal_start[[1]], lag_time = mal_start[[2]], 
                           growth_rate = mal_start[[3]]))
  
  mal_mod_pars <- mal_mod$m$getPars()
<<<<<<< HEAD

#APPLYING NLR MODEL TO THE DATA  
  
  #Run the DNC method on the data first
  if(exists('master_DNC') == FALSE) {
    replay(evaluate(file('../data_cleaning_methods/DNC.R')))
  }
  
  NLR <- master_DNC 
    
#Apply these NLR models to the uncleaned data (with duplicates removed) for each sex and calculate prediction intervals that are 4
=======
  
  
#Apply these models to the uncleaned data and calculate prediction intervals that are 4
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
#times the standard deviation from the predicted measurement
  females2 <- subset(NLR, NLR$sex == 'Female')
  
  females2 <- females2 %>%
<<<<<<< HEAD
    mutate(predicted_measurement=predict(fem_mod, newdata = females2),
           sd_pred = sd(predicted_measurement),
           upper_int = predicted_measurement + (model_var_cutoff*(sd(predicted_measurement))),
           lower_int = predicted_measurement - (model_var_cutoff*(sd(predicted_measurement))))      
=======
    mutate(NLR_pred=predict(fem_mod, newdata = females2),
           sd_pred = sd(NLR_pred),
           upper_int = NLR_pred + (model_var_cutoff*(sd(NLR_pred))),
           lower_int = NLR_pred - (model_var_cutoff*(sd(NLR_pred))))      
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  
  males2 <- subset(NLR, NLR$sex == 'Male')
  
  males2 <- males2 %>%
<<<<<<< HEAD
    mutate(predicted_measurement=predict(mal_mod, newdata = males2),
           sd_pred = sd(predicted_measurement),
           upper_int = predicted_measurement + (model_var_cutoff*(sd(predicted_measurement))),
           lower_int = predicted_measurement - (model_var_cutoff*(sd(predicted_measurement)))) 
  
  NLR <- rbind(males2, females2)

=======
    mutate(NLR_pred=predict(mal_mod, newdata = males2),
           sd_pred = sd(NLR_pred),
           upper_int = NLR_pred + (model_var_cutoff*(sd(NLR_pred))),
           lower_int = NLR_pred - (model_var_cutoff*(sd(NLR_pred)))) 
  
  NLR <- rbind(males2, females2)
  
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  #get_outliers is a function that finds possible outliers in the data by identifying values that
  #are ouside the upper and lower prediction intervals given by the model and then 
  #saves the outcome of this logic to a new variable in the dataframe that is named
  #after the type of outlier being tested
  get_outliers <- function(X, var1 = "new_weight", upper_limit = "upper_int", 
                           lower_limit = "lower_int", outlier_name = "outlier", print_results = TRUE) {
    X[[paste(var1, "_", outlier_name, sep = "")]] <- X[[var1]] > X[[upper_limit]] | X[[var1]] < X[[lower_limit]]
    if(print_results == TRUE) {
      print(paste(var1, " ", outlier_name, "s", sep = ""))
      print(sum(X[[paste(var1, "_", outlier_name, sep = "")]]), na.rm = TRUE)
    }
    return(X)
  }
<<<<<<< HEAD

  #check the duplications and outliers
  NLR <- get_duplications(NLR)
  NLR <- get_outliers(NLR)
  
  
  #find the first and last observation in each group of duplicates
  NLR<- NLR %>%
=======
  
  NLR <- get_outliers(NLR)
  
#Apply step 1 of algorithm to delete complete duplicates, keeping just the oldest
#duplicate in the set of data entries  
  
  NLR <- get_duplications(NLR)
  
  NLR <- NLR %>%
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
<<<<<<< HEAD
    ungroup() %>%
    select(-c(last_obs_num, first_obs_num))
  
  #Remove duplications by keeping the last (most recent) observation
  NLR <- NLR %>%
    filter(duplications == FALSE | duplications == TRUE & last_observation == TRUE)
  
  #check for any remaining duplications
=======
    ungroup()
  
  NLR <- NLR %>%
    filter(complete_duplications == FALSE | complete_duplications == TRUE & first_observation == TRUE)
  
  #check the difference in duplications
  NLR <- get_duplications(NLR)
 
#Apply step 2 of algorithm to delete duplicates that are not complete duplicates
#by keeping only the duplicate that is closest to the predicted measurement    

#Work out which of the remaining duplicates are closest to the predicted measurements   
  NLR <- NLR %>%
    mutate(abs_diff_from_pred = abs(new_weight - NLR_pred)) %>%
    group_by(dups_ID) %>%
    mutate(min_diff = min(abs_diff_from_pred)) %>%
    ungroup() %>%
    mutate(smallest_diff_from_pred = case_when(min_diff == abs_diff_from_pred ~ TRUE,
                                               min_diff != abs_diff_from_pred ~ FALSE))
  
  NLR <- NLR %>%
    filter(duplications == FALSE | duplications == TRUE & smallest_diff_from_pred == TRUE)
  
  #check the difference in duplications and outliers
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  NLR <- get_duplications(NLR)
  NLR <- get_outliers(NLR)
  
#cut out remaining outliers identified by the prediction intervals to clean the data
  master_NLR <- NLR %>%
    filter(new_weight_outlier == FALSE)
