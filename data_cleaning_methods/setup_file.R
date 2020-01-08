#Parameter set up

#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script sets up the parameters for the cut_offs and starting values
#needed for certain data cleaning methods

#LIBRARIES:
library(tidyverse)
library(evaluate)

<<<<<<< HEAD
#This function sets up the externally defined cut-offs (GCO method)
#It should be modified to define which values are impossible based on the particular dataset

general_cutoffs <- function(x, lower_limit = 0.5, upper_limit = 250, age_min_value = 1825, lower_limit_age_min = 10, gco_cut = 76.9){
  x <- x %>%
    mutate(lower_int = case_when(age < age_min_value ~ lower_limit,
                                 age >= age_min_value ~ lower_limit_age_min),
           upper_int = upper_limit,
           cut_outlier = new_weight < lower_int |
             new_weight > upper_int,
           predicted_measurement = gco_cut)
  return(x)
}

#Algorithm cutoffs

algo_cutoffs <- function(x, lower_limit = 0.5, upper_limit = 250, age_min_value = 1825, lower_limit_age_min = 10){
  x <- x %>%
    mutate(lower_int2 = case_when(age < age_min_value ~ lower_limit,
                                 age >= age_min_value ~ lower_limit_age_min),
           upper_int2 = upper_limit,
           general_outlier = new_weight < lower_int2 |
             new_weight > upper_int2)
  return(x)
}

#This function sets up the internally defined cut-off for SZCO method
z_score_cutoffs <- function(x, z_score_cut = 3){
  x <- x %>%
    mutate(predicted_measurement = mean(new_weight),
           lower_int = case_when(predicted_measurement - (z_score_cut*sd(weight)) >= 0 ~ predicted_measurement - (z_score_cut*sd(weight)),
                                 predicted_measurement - (z_score_cut*sd(weight)) < 0 ~ 0),
           upper_int = predicted_measurement + (z_score_cut*sd(weight)),
           z_outlier = new_weight < lower_int |
             new_weight > upper_int)
  return(x)
}


=======
#This function sets up the externally defined cut-offs for different data cleaning methods (GCO, NLR-A and NLME-A)
#It should be modified to define which values are impossible based on the particular dataset

general_cutoffs <- function(x, lower_limit = 0.5, upper_limit = 250, age_min_value = 1825, lower_limit_age_min = 10){
  x$cut_outlier <- (x$new_weight < lower_limit) | 
    ((x$new_weight < lower_limit_age_min) & (x$age >= age_min_value)) | 
    (x$new_weight > upper_limit)
  return(x)
}

#the internally defined cut-off for z scores used in the SZCO and LZCO methods
z_score_cutoff <- 3
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192

#The cut offs for age interval and number of data entries required within the age interval/bin
#used in the LZCO, NLME and NLME-A methods

age_interval_cutoff <- 365

bin_freq_cutoff <- 100

<<<<<<< HEAD

#This function sets up the internally defined cut-off for LZCO method
z_score_cutoffs2 <- function(x, z_score_cut = 3){
  x <- x %>%
    group_by(bins, sex) %>%
    mutate(predicted_measurement = mean(new_weight),
           lower_int = case_when(predicted_measurement - (z_score_cut*sd(weight)) >= 0 ~ predicted_measurement - (z_score_cut*sd(weight)),
                                 predicted_measurement - (z_score_cut*sd(weight)) < 0 ~ 0),
           upper_int = predicted_measurement + (z_score_cut*sd(weight)),
           z_outlier = new_weight < lower_int |
             new_weight > upper_int) %>%
    ungroup()
  return(x)
}


#Formula for NLR and NLME growth models        
f1 <- new_weight ~ (Asym*exp(-exp(((growth_rate*exp(1))/Asym)*(lag_time-age)+1)))

#starting values for NLR and NLME growth models       
=======
#Formula for NLR and NLME growth models        
f1 <- new_weight ~ (Asym*exp(-exp(((growth_rate*exp(1))/Asym)*(lag_time-age)+1)))

#tarting values for NLR and NLME growth models       
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192

#set up starting values for regression models based on the original data without simulated errors
#starting values for these models were defined by externally published values

orig_dat <- read_csv('../data/reformatted_CLOSER_data.csv')

females0 <- subset(orig_dat, orig_dat$sex == 'Female')
fem_mod0 <- nls(f1,
                data = females0,
                start = c(Asym = 70.2, lag_time = 0, growth_rate = 0.1))

fem_start <- fem_mod0$m$getPars()

males0 <- subset(orig_dat, orig_dat$sex == 'Male')
mal_mod0 <- nls(f1,
                data = males0,
                start = c(Asym = 83.6, lag_time = 0, growth_rate = 0.1))

mal_start <- mal_mod0$m$getPars()

#The cut off for the amount of variance from the predicted values given by the models,
#in order to generate prediction intervals from them

model_var_cutoff <- 4

<<<<<<< HEAD
#this sets up the approproate metric and imperial unit conversions for weight/height measurements

metric_conv <- 0.45359237
imperial_conv <- 2.2046226218   


#list of numbers that should not be transposed due to their difference with the
#original number being too small (9 in this case)
bad_numbers <- c(12,21,23,32,34,43,45,54,56,65,67,76,78,87,89,98)
=======

>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192


