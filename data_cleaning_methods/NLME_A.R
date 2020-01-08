#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using a non-linear mixed effects model for growth and a 5 step algorithm to identify outliers

<<<<<<< HEAD
#METHOD NAME: Non-linear mixed effects model with algorithm (NLME-A)
=======
#METHOD NAME: Non-linear mixed effects model with algorithm (NLR-A)
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192

#LIBRARIES:
  library(evaluate)
  library(Rcpp)
<<<<<<< HEAD
  library(nlme)
  library(modelr)
=======
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  
#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively
  
<<<<<<< HEAD
  #MAKING AN APPOPRIATE NLME MODEL (ONLY DO THIS IF NOT ALREADY LOADED AS TAKES TIME)
  
  #Run the GCO method on the data first, to obtain a cleaner subset of the data
  #on which to run the model
  if(exists('mixd') == FALSE) {
  
    #Run the NLR AND GCO methods on the data first, to obtain a cleaner subset of the data
    #on which to run the model
    
    if(exists('master_NLR_A') == FALSE) {
      replay(evaluate(file('../data_cleaning_methods/NLR_A.R')))
    }
    
    subs <- get_outliers(dat2) %>%
      mutate(ind_ID = as.factor(ind_ID),
             sex = as.factor(sex)) %>%
      filter(new_weight_outlier == FALSE)
    
    #set up starting values based on NLR model and run the NLME model  
    start_values <- c(as.numeric(fem_mod_pars[1]),as.numeric(mal_mod_pars[1]),
                      as.numeric(fem_mod_pars[2]),as.numeric(mal_mod_pars[2]),
                      as.numeric(fem_mod_pars[3]),as.numeric(mal_mod_pars[3]))
    
    mixd <- nlme(f1,
                 data = subs,
                 fixed = list(Asym + lag_time + growth_rate ~ 1 + sex),
                 random = pdDiag(Asym + lag_time + growth_rate ~ 1),
                 groups = ~ ind_ID,
                 start = start_values)
    
    #To calculate prediction intervals, data is divided into 365 day age periods
    #Function that will get appropriate time intervals according to the data
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
    
    #Set initial intervals with the ideal time period required and run the function
    intervals <- seq(min(dat$age), max(dat$age+age_interval_cutoff), age_interval_cutoff)
    bin_freq <- get_intervals(dat, intervals, bin_freq_cutoff)
    
    #get the appropriate intervals from the new data
    intervals <- sort(unique(as.numeric(c(min(intervals), as.numeric(bin_freq$lower_bin), max(intervals)))))  
    
    
    #get the appropriate intervals from the new data
    subs <- subs %>%
      mutate(bins = cut(subs$age, breaks = intervals, include.lowest = TRUE),
             bins2 = gsub("\\[|\\]|\\(|\\)", "", bins)) %>%
      separate(bins2, c("lower_bin", "upper_bin"), ",") %>%
      mutate(middle_age = (as.numeric(lower_bin)+as.numeric(upper_bin))/2)
    
    
    #get variation for prediction intervals
    grid_mod_variation <- subs %>%
      mutate(ind_ID = factor(ind_ID)) %>%
      data_grid(ind_ID = unique(ind_ID),
                age = unique(middle_age),
                sex = unique(sex)) %>%
      mutate(pred_RE = predict(mixd, newdata = ., level = 1),
             bins = cut(age, breaks = intervals, include.lowest = TRUE)) %>%
      rename(middle_age = age) %>%
      group_by(middle_age, bins, sex) %>%
      summarise(sd_pred_RE = as.numeric(sd(pred_RE))) %>%
      ungroup() %>%
      select(bins,middle_age,sex,sd_pred_RE) 
    
    grid_resid_variation <- as.tbl(subs) %>%
      mutate(pred_RE = predict(mixd, newdata = ., level = 1),
             residuals = pred_RE - weight) %>%
      group_by(middle_age, bins, sex) %>%
      summarise(sd_residuals = as.numeric(sd(residuals))) %>%
      ungroup() %>%
      select(bins,middle_age,sex,sd_residuals) %>%
      filter(!is.na(bins))
    
    #functions that smooth over residuals
    aicc.loess <- function(fit) {
      stopifnot(inherits(fit, 'loess'))
      n <- fit$n
      trace <- fit$trace.hat
      sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
      return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
    }
    
    autoloess <- function(fit, span=c(0.1,2), print_results = TRUE) {
      stopifnot(inherits(fit, 'loess'), length(span) == 2)
      f <- function(span) aicc.loess(update(fit, span=span))
      fit <- update(fit, span=optimize(f, span)$minimum)
      if(print_results == TRUE) {
        print(c('optimised span', fit$pars$span))
      }
      return(fit)
    }
    
    males2 <- subset(grid_resid_variation, grid_resid_variation$sex == 'Male')
    male_loess <- autoloess(loess(sd_residuals ~ middle_age, data = males2), print_results = FALSE)
    male_smoothed <- predict(male_loess)
    males2 <- males2 %>%
      mutate(smoothed_sd_residuals = male_smoothed)
    
    females2 <- subset(grid_resid_variation, grid_resid_variation$sex == 'Female')
    female_loess <- autoloess(loess(sd_residuals ~ middle_age, data = females2), print_results = FALSE)
    female_smoothed <- predict(female_loess)
    females2 <- females2 %>%
      mutate(smoothed_sd_residuals = female_smoothed)
    
    grid_merged <- grid_mod_variation %>% 
      full_join(rbind(females2,males2)) %>%
      mutate(total_variation = sd_pred_RE + smoothed_sd_residuals) %>%
      select(bins,middle_age,sex,sd_pred_RE,sd_residuals,smoothed_sd_residuals,total_variation)
  
    
  }
  
  #APPLYING NLME MODEL TO THE DATA  
  
  #Get uncleaned data
  if(exists('master_DNC') == FALSE) {
    replay(evaluate(file('../data_cleaning_methods/DNC.R')))
  }
  
  NLME <- master_DNC
  
  #add prediction intervals to the uncleaned data 
  
  NLME <- NLME %>%
    mutate(bins = cut(age, breaks = intervals, include.lowest = TRUE)) %>%
    full_join(grid_merged, by = c("bins", "sex")) %>%
    mutate(random_effects = predict(mixd, newdata = NLME, level = 1),
           fixed_effects = predict(mixd, newdata = NLME, level = 0),
           predicted_measurement = case_when(!is.na(random_effects) & num_data_entries > 1 ~ random_effects,
                                             !is.na(random_effects) & num_data_entries < 2 ~ fixed_effects,
                                             is.na(random_effects) ~ fixed_effects),
           upper_rand = predicted_measurement + (model_var_cutoff*smoothed_sd_residuals),
           lower_rand = predicted_measurement - (model_var_cutoff*smoothed_sd_residuals),
           upper_fixd = predicted_measurement + (model_var_cutoff*total_variation),
           lower_fixd = predicted_measurement - (model_var_cutoff*total_variation),
           upper_int = case_when(!is.na(random_effects) & num_data_entries > 1 ~ upper_rand,
                                 !is.na(random_effects) & num_data_entries < 2 ~ upper_fixd,
                                 is.na(random_effects) ~ upper_fixd),
           lower_int = case_when(!is.na(random_effects) & num_data_entries > 1 ~ lower_rand,
                                 !is.na(random_effects) & num_data_entries < 2 ~ lower_fixd,
                                 is.na(random_effects) ~ lower_fixd))
  
  
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
  
  #check the duplications and outliers
  NLME <- get_duplications(NLME)
  NLME <- get_outliers(NLME)
  
  
  #find the first and last observation in each group of duplicates
  NLME <- NLME %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup() %>%
    select(-c(last_obs_num, first_obs_num))
  
  #Apply step 1 of algorithm to delete complete duplicates, keeping just the oldest
  #duplicate in the set of data entries    
  
  dat1 <- NLME %>%
    filter(complete_duplications == FALSE | complete_duplications == TRUE & first_observation == TRUE)
  
  #check the difference in duplications
  dat1 <- get_duplications(dat1)
  
  #Apply step 2 of algorithm to delete duplicates that are not complete duplicates
  #by keeping only the duplicate that is closest to the predicted measurement     
  
  #Work out which of the remaining duplicates are closest to the predicted measurements   
  dat2 <- dat1 %>%
    mutate(abs_diff_from_pred = abs(new_weight - predicted_measurement)) %>%
    group_by(dups_ID) %>%
    mutate(min_diff = min(abs_diff_from_pred)) %>%
    ungroup() %>%
    mutate(smallest_diff_from_pred = case_when(min_diff == abs_diff_from_pred ~ TRUE,
                                               min_diff != abs_diff_from_pred ~ FALSE))
  
  dat2 <- dat2 %>%
    filter(duplications == FALSE | duplications == TRUE & smallest_diff_from_pred == TRUE)
  
  #check the difference in duplications
  dat2 <- get_duplications(dat2)
  
  
  #Apply step 3 of algorithm to identify the cause of possible errors in dataset
  
  #Function to transpose numbers  
=======
#Run the NLME method on the data first to perform the first 2 steps of the algorithm
#(just removing the duplications)

  if(exists('master_NLME') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/NLME.R')))
  }
  
#Apply step 3 of algorithm to identify the cause of possible errors in dataset
  
#Function to transpose numbers  
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  cppFunction('IntegerVector Reverse_CPP2(IntegerVector x) {
              int n = x.size();
              IntegerVector out(n);
              IntegerVector xx = clone(x);
              
              for (int i = 0; i < n; ++i){
              int reverse = 0;
              while(xx[i] != 0) {
              int remainder = xx[i]%10;
              reverse = reverse*10 + remainder;
              xx[i]/= 10;
              }
              out[i] = reverse;
              }
              
              return out;
              
              }')
<<<<<<< HEAD
  
  dat3 <- dat2 %>%
=======
   
#list of numbers that should not be transposed due to their difference with the
#original number being too small (9 in this case)
  bad_numbers <- c(12,21,23,32,34,43,45,54,56,65,67,76,78,87,89,98)
  
  dat3 <- NLME %>%
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    mutate(div10_weights = new_weight/10,
           mul10_weights = new_weight*10,
           div100_weights = new_weight/100,
           mul100_weights = new_weight*100,
           div1000_weights = new_weight/1000,
           mul1000_weights = new_weight*1000,
           min100_weights = new_weight - 100, 
           min1000_weights = new_weight - 1000, 
           add100_weights = new_weight + 100, 
           add1000_weights = new_weight + 1000, 
<<<<<<< HEAD
           metric_weights = round(as.numeric(weight*metric_conv), 2),
           imperial_weights = round(as.numeric(weight*imperial_conv), 2),
=======
           kg_weights = round(as.numeric(weight*0.45359237), 2),
           lbs_weights = round(as.numeric(weight*2.2046226218), 2),
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
           weight_floored = floor(weight),
           reversed_weights1 = as.numeric(Reverse_CPP2(weight_floored)),
           reversed_weights = case_when(weight_floored %in% bad_numbers == TRUE ~ Inf,
                                        weight_floored %in% bad_numbers == FALSE ~ reversed_weights1 + (weight %% 1)),
<<<<<<< HEAD
           div10_weights_diff = abs(div10_weights - predicted_measurement),
           mul10_weights_diff = abs(mul10_weights - predicted_measurement),
           div100_weights_diff = abs(div100_weights - predicted_measurement),
           mul100_weights_diff = abs(mul100_weights - predicted_measurement),
           div1000_weights_diff = abs(div1000_weights - predicted_measurement),
           mul1000_weights_diff = abs(mul1000_weights - predicted_measurement),
           min100_weights_diff = abs(min100_weights - predicted_measurement),
           min1000_weights_diff = abs(min1000_weights - predicted_measurement),
           add100_weights_diff = abs(add100_weights - predicted_measurement),
           add1000_weights_diff = abs(add1000_weights - predicted_measurement),
           metric_weights_diff = abs(metric_weights - predicted_measurement),
           imperial_weights_diff = abs(imperial_weights - predicted_measurement),
           reversed_weights_diff = abs(reversed_weights - predicted_measurement)) %>%
=======
           div10_weights_diff = abs(div10_weights - both_effects),
           mul10_weights_diff = abs(mul10_weights - both_effects),
           div100_weights_diff = abs(div100_weights - both_effects),
           mul100_weights_diff = abs(mul100_weights - both_effects),
           div1000_weights_diff = abs(div1000_weights - both_effects),
           mul1000_weights_diff = abs(mul1000_weights - both_effects),
           min100_weights_diff = abs(min100_weights - both_effects),
           min1000_weights_diff = abs(min1000_weights - both_effects),
           add100_weights_diff = abs(add100_weights - both_effects),
           add1000_weights_diff = abs(add1000_weights - both_effects),
           kg_weights_diff = abs(kg_weights - both_effects),
           lbs_weights_diff = abs(lbs_weights - both_effects),
           reversed_weights_diff = abs(reversed_weights - both_effects)) %>%
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    transform(smallest_diff = pmin((div10_weights_diff),
                                   (mul10_weights_diff), (div100_weights_diff),
                                   (mul100_weights_diff), (div1000_weights_diff),
                                   (mul1000_weights_diff), (min100_weights_diff), (min1000_weights_diff),
                                   (add100_weights_diff), (add1000_weights_diff),
<<<<<<< HEAD
                                   (metric_weights_diff), (imperial_weights_diff),
=======
                                   (kg_weights_diff), (lbs_weights_diff),
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
                                   reversed_weights_diff)) %>%
    mutate(div10_smallest_diff = (div10_weights_diff) == smallest_diff,
           mul10_smallest_diff = (mul10_weights_diff) == smallest_diff,
           div100_smallest_diff = (div100_weights_diff) == smallest_diff,
           mul100_smallest_diff = (mul100_weights_diff) == smallest_diff,
           div1000_smallest_diff = (div1000_weights_diff) == smallest_diff,
           mul1000_smallest_diff = (mul1000_weights_diff) == smallest_diff,
           min100_smallest_diff = (min100_weights_diff) == smallest_diff,
           min1000_smallest_diff = (min1000_weights_diff) == smallest_diff,
           add100_smallest_diff = (add100_weights_diff) == smallest_diff,
           add1000_smallest_diff = (add1000_weights_diff) == smallest_diff,
<<<<<<< HEAD
           metric_smallest_diff = (metric_weights_diff) == smallest_diff,
           imperial_smallest_diff = (imperial_weights_diff) == smallest_diff,
           reversed_smallest_diff = (reversed_weights_diff) == smallest_diff)
  #function that replaces outliers with the correction that reults in the 
  #smallest difference from the predicted measurement
  
  replace_outliers <- function(X, var1, var2, print_results = TRUE) {
    a <- sum(X[[paste(var1, "_outlier", sep = "")]])
    X[[var1]] <- ifelse(X[[paste(var1, "_outlier", sep = "")]] == TRUE &
                          X[[paste(var2, "_weights_outlier", sep = "")]] == FALSE &
                          X[[paste(var2, "_smallest_diff", sep = "")]] == TRUE, 
                        X[[paste(var2, "_weights", sep = "")]], X[[var1]])
    X <- get_outliers(X, print_results = FALSE)
    b <- sum(X[[paste(var1, "_outlier", sep = "")]])
    if(print_results == TRUE) {
      print(paste("Total number of outliers corrected"))
      print(a-b)
    }
    return(X)
  }
=======
           kg_smallest_diff = (kg_weights_diff) == smallest_diff,
           lbs_smallest_diff = (lbs_weights_diff) == smallest_diff,
           reversed_smallest_diff = (reversed_weights_diff) == smallest_diff)
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  
  #get_outliers is a function that finds possible outliers in the data by identifying values that
  #are ouside the upper and lower prediction intervals given by the model and then 
  #saves the outcome of this logic to a new variable in the dataframe that is named
  #after the type of outlier being tested
<<<<<<< HEAD
  get_outliers <- function(X, var1 = "new_weight", upper_limit = "upper_int", 
                           lower_limit = "lower_int", outlier_name = "outlier", print_results = TRUE) {
    X[[paste(var1, "_", outlier_name, sep = "")]] <- X[[var1]] > X[[upper_limit]] | X[[var1]] < X[[lower_limit]]
    if(print_results == TRUE) {
      print(paste(var1, " ", outlier_name, "s", sep = ""))
      print(sum(X[[paste(var1, "_", outlier_name, sep = "")]]), na.rm = TRUE)
    }
    return(X)
  }
=======
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  
  #get the outliers for each possible correction
  dat3 <- get_outliers(dat3)
  dat3 <- get_outliers(dat3, var1 = "div10_weights")
  dat3 <- get_outliers(dat3, var1 = "mul10_weights")
  dat3 <- get_outliers(dat3, var1 = "div100_weights")
  dat3 <- get_outliers(dat3, var1 = "mul100_weights")
  dat3 <- get_outliers(dat3, var1 = "div1000_weights")
  dat3 <- get_outliers(dat3, var1 = "mul1000_weights")
<<<<<<< HEAD
  dat3 <- get_outliers(dat3, var1 = "metric_weights")
  dat3 <- get_outliers(dat3, var1 = "imperial_weights")
=======
  dat3 <- get_outliers(dat3, var1 = "kg_weights")
  dat3 <- get_outliers(dat3, var1 = "lbs_weights")
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  dat3 <- get_outliers(dat3, var1 = "reversed_weights")
  dat3 <- get_outliers(dat3, var1 = "min100_weights")
  dat3 <- get_outliers(dat3, var1 = "min1000_weights")
  dat3 <- get_outliers(dat3, var1 = "add100_weights")
  dat3 <- get_outliers(dat3, var1 = "add1000_weights")
  
<<<<<<< HEAD
=======
  #replace outliers is a function that replaces outliers with the correction that reults in the 
  #smallest difference from the predicted measurement
  
  replace_outliers <- function(X, var1, var2, print_results = TRUE) {
    a <- sum(X[[paste(var1, "_outlier", sep = "")]])
    X[[var1]] <- ifelse(X[[paste(var1, "_outlier", sep = "")]] == TRUE &
                          X[[paste(var2, "_weights_outlier", sep = "")]] == FALSE &
                          X[[paste(var2, "_smallest_diff", sep = "")]] == TRUE, 
                        X[[paste(var2, "_weights", sep = "")]], X[[var1]])
    X <- get_outliers(X, print_results = FALSE)
    b <- sum(X[[paste(var1, "_outlier", sep = "")]])
    if(print_results == TRUE) {
      print(paste("Total number of outliers corrected"))
      print(a-b)
    }
    return(X)
  }
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  
  #Make corrections
  dat3 <- replace_outliers(dat3, "new_weight", "div10")
  dat3 <- replace_outliers(dat3, "new_weight", "div100")  
  dat3 <- replace_outliers(dat3, "new_weight", "div1000")  
  dat3 <- replace_outliers(dat3, "new_weight", "mul10")
  dat3 <- replace_outliers(dat3, "new_weight", "mul100")  
  dat3 <- replace_outliers(dat3, "new_weight", "mul1000")  
  dat3 <- replace_outliers(dat3, "new_weight", "min100")
  dat3 <- replace_outliers(dat3, "new_weight", "min1000")
  dat3 <- replace_outliers(dat3, "new_weight", "add100")
  dat3 <- replace_outliers(dat3, "new_weight", "add1000")
<<<<<<< HEAD
  dat3 <- replace_outliers(dat3, "new_weight", "metric")
  dat3 <- replace_outliers(dat3, "new_weight", "imperial")  
  dat4 <- replace_outliers(dat3, "new_weight", "reversed")  
  
  #Apply step 4 of algorithm to identify consecutive values that jump in size
  
  #Function that identifies consecutive values that jump in size more than
  #the maximum predicted amount that they could change
  
=======
  dat3 <- replace_outliers(dat3, "new_weight", "kg")
  dat3 <- replace_outliers(dat3, "new_weight", "lbs")  
  dat4 <- replace_outliers(dat3, "new_weight", "reversed")  
  
#Apply step 4 of algorithm to identify consecutive values that jump in size

  #Get jumpers is a function that identifies consecutive values that jump in size more than
  #the maximum predicted amount that they could change
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
  get_jumpers <- function(X) {
    X <- X %>%
      group_by(ind_ID) %>%
      mutate(weight_diff_lag = abs(new_weight - lag(new_weight)),
             weight_diff_lead = abs(new_weight - lead(new_weight)),
             weight_diff_accum = weight_diff_lead + weight_diff_lag,
             pred_diff_lag = abs(upper_int - lag(lower_int)),
             pred_diff_lead = abs(lower_int - lead(upper_int)),
             pred_diff_accum = pred_diff_lag + pred_diff_lead,
             jumper = weight_diff_lag > pred_diff_lag |
               weight_diff_lead > pred_diff_lead) %>%
      ungroup()
    X <- subset(X, !(X$new_weight_outlier == TRUE & X$jumper == TRUE))
    return(X)
  }  
  
  success <- FALSE
  
  while (!success) {
    dat4 <- dat4
    size_of_dataset_before <- length(dat4$ind_ID)
    dat4 <- get_jumpers(dat4)
    size_of_dataset_after <- length(dat4$ind_ID)
    size_of_dataset_before == size_of_dataset_after
    success <- (size_of_dataset_before == size_of_dataset_after) == TRUE
  }
  
  #look at difference
  dat4 <- get_duplications(dat4)
  dat4 <- get_outliers(dat4) 
  
<<<<<<< HEAD
  #Apply step 5 of algorithm to remove any remaining implausible values 
  
  dat4 <- algo_cutoffs(dat4)
  
  
  #remove values defined as outliers  
  master_NLME_A <- dat4 %>%
    filter(general_outlier == FALSE)
  
  
=======
#Apply step 5 of algorithm to remove any remaining implausible values 

  #define the externally defined cut-offs for weight that are used to identify outliers
  dat4 <- general_cutoffs(dat4)
  
  #remove values defined as outliers  
  master_NLME_A <- dat4 %>%
    filter(cut_outlier == FALSE)
  
  
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
