#Parameter set up

#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script sets up the parameters for the cut_offs and starting values
#needed for certain data cleaning methods

#LIBRARIES:
library(tidyverse)
library(evaluate)

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

#The cut offs for age interval and number of data entries required within the age interval/bin
#used in the LZCO, NLME and NLME-A methods

age_interval_cutoff <- 365

bin_freq_cutoff <- 100

#Formula for NLR and NLME growth models        
f1 <- new_weight ~ (Asym*exp(-exp(((growth_rate*exp(1))/Asym)*(lag_time-age)+1)))

#tarting values for NLR and NLME growth models       

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




