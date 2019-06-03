#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using a non-linear mixed effects model for growth to identify outliers

#METHOD NAME: Non-linear mixed effects model (NLME)

#LIBRARIES:
  library(evaluate)
  library(nlme)
  library(modelr)

#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively
  
#Run the NLR method on the data first, to obtain a cleaner subset of the data
#on which to run the model

  if(exists('master_NLR') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/NLR.R')))
  }
  
  sub_dat <- master_NLR %>%
    mutate(ind_ID = as.factor(ind_ID),
           sex = as.factor(sex))
  
  NLME <- master_DNC

#set up starting values based on NLR model and run the NLME model  
  start_values <- c(as.numeric(fem_mod_pars[1]),as.numeric(mal_mod_pars[1]),
                    as.numeric(fem_mod_pars[2]),as.numeric(mal_mod_pars[2]),
                    as.numeric(fem_mod_pars[3]),as.numeric(mal_mod_pars[3]))
  
  mixd <- nlme(f1,
               data = sub_dat,
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
  sub_dat <- sub_dat %>%
    mutate(bins = cut(sub_dat$age, breaks = intervals, include.lowest = TRUE),
           bins2 = gsub("\\[|\\]|\\(|\\)", "", bins)) %>%
    separate(bins2, c("lower_bin", "upper_bin"), ",") %>%
    mutate(middle_age = (as.numeric(lower_bin)+as.numeric(upper_bin))/2)


#get variation for prediction intervals
  grid_mod_variation <- sub_dat %>%
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
  
  grid_resid_variation <- as.tbl(sub_dat) %>%
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

#add prediction intervals to original data

  grid_merged <- grid_mod_variation %>% 
    full_join(rbind(females2,males2)) %>%
    mutate(total_variation = sd_pred_RE + smoothed_sd_residuals) %>%
    select(bins,middle_age,sex,sd_pred_RE,sd_residuals,smoothed_sd_residuals,total_variation)


  NLME <- NLME %>%
    mutate(bins = cut(age, breaks = intervals, include.lowest = TRUE)) %>%
    full_join(grid_merged, by = c("bins", "sex")) %>%
    mutate(random_effects = predict(mixd, newdata = NLME, level = 1),
           fixed_effects = predict(mixd, newdata = NLME, level = 0),
           both_effects = case_when(!is.na(random_effects) & num_data_entries > 1 ~ random_effects,
                                    !is.na(random_effects) & num_data_entries < 2 ~ fixed_effects,
                                    is.na(random_effects) ~ fixed_effects),
           upper_rand = both_effects + (model_var_cutoff*smoothed_sd_residuals),
           lower_rand = both_effects - (model_var_cutoff*smoothed_sd_residuals),
           upper_fixd = both_effects + (model_var_cutoff*total_variation),
           lower_fixd = both_effects - (model_var_cutoff*total_variation),
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
  
  NLME <- get_outliers(NLME, print_results = TRUE)

#Apply step 1 of algorithm to delete complete duplicates, keeping just the oldest
#duplicate in the set of data entries  
   
  NLME <- get_duplications(NLME)
  
  NLME <- NLME %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup()
  
  NLME <- NLME %>%
    filter(complete_duplications == FALSE | complete_duplications == TRUE & first_observation == TRUE)

#look at difference
  NLME <- get_duplications(NLME)
  NLME <- get_outliers(NLME) 

#Apply step 2 of algorithm to delete duplicates that are not complete duplicates
#by keeping only the duplicate that is closest to the predicted measurement    

  NLME <- NLME %>%
    mutate(abs_diff_from_pred = abs(new_weight - both_effects)) %>%
    group_by(dups_ID) %>%
    mutate(min_diff = min(abs_diff_from_pred)) %>%
    ungroup() %>%
    mutate(smallest_diff_from_pred = case_when(min_diff == abs_diff_from_pred ~ TRUE,
                                               min_diff != abs_diff_from_pred ~ FALSE))
  
  NLME <- NLME %>%
    filter(duplications == FALSE | duplications == TRUE & smallest_diff_from_pred == TRUE)
  
#check the difference in duplications and outliers
  NLME <- get_duplications(NLME)
  NLME <- get_outliers(NLME)
  
#cut out remaining outliers identified by the prediction intervals to clean the data
  master_NLME <- NLME %>%
    filter(new_weight_outlier == FALSE)
  
  
  