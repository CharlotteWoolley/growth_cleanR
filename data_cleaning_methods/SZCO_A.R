#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an internally defined cut-off (cross-sectional z-score) and a 5 step algorithm to identify outliers

#METHOD NAME: Standard z-score cut-off with algorithm (SZCO-A)

#LIBRARIES:
library(evaluate)
library(Rcpp)

#replay(evaluate) runs the code within a file and returnes the output as if it had
#been run in the console. This is useful for workflows such as this where multiple files
#are linked together and rely on eachother to work effectively

#Get uncleaned data
if(exists('master_DNC') == FALSE) {
  replay(evaluate(file('../data_cleaning_methods/DNC.R')))
}

#define the z-scores for weight that are used to identify outliers
#The cut off for z-score is given as more than 3 or less than -3
SZCO <- z_score_cutoffs(master_DNC)

#check for any duplications
SZCO <- get_duplications(SZCO)

#find the first and last observation in each group of duplicates
SZCO<- SZCO %>%
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

dat1 <- SZCO %>%
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
  
dat3 <- dat2 %>%
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
         metric_weights = round(as.numeric(weight*metric_conv), 2),
         imperial_weights = round(as.numeric(weight*imperial_conv), 2),
         weight_floored = floor(weight),
         reversed_weights1 = as.numeric(Reverse_CPP2(weight_floored)),
         reversed_weights = case_when(weight_floored %in% bad_numbers == TRUE ~ Inf,
                                      weight_floored %in% bad_numbers == FALSE ~ reversed_weights1 + (weight %% 1)),
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
  transform(smallest_diff = pmin((div10_weights_diff),
                                 (mul10_weights_diff), (div100_weights_diff),
                                 (mul100_weights_diff), (div1000_weights_diff),
                                 (mul1000_weights_diff), (min100_weights_diff), (min1000_weights_diff),
                                 (add100_weights_diff), (add1000_weights_diff),
                                 (metric_weights_diff), (imperial_weights_diff),
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

#get the outliers for each possible correction
dat3 <- get_outliers(dat3)
dat3 <- get_outliers(dat3, var1 = "div10_weights")
dat3 <- get_outliers(dat3, var1 = "mul10_weights")
dat3 <- get_outliers(dat3, var1 = "div100_weights")
dat3 <- get_outliers(dat3, var1 = "mul100_weights")
dat3 <- get_outliers(dat3, var1 = "div1000_weights")
dat3 <- get_outliers(dat3, var1 = "mul1000_weights")
dat3 <- get_outliers(dat3, var1 = "metric_weights")
dat3 <- get_outliers(dat3, var1 = "imperial_weights")
dat3 <- get_outliers(dat3, var1 = "reversed_weights")
dat3 <- get_outliers(dat3, var1 = "min100_weights")
dat3 <- get_outliers(dat3, var1 = "min1000_weights")
dat3 <- get_outliers(dat3, var1 = "add100_weights")
dat3 <- get_outliers(dat3, var1 = "add1000_weights")


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
dat3 <- replace_outliers(dat3, "new_weight", "metric")
dat3 <- replace_outliers(dat3, "new_weight", "imperial")  
dat4 <- replace_outliers(dat3, "new_weight", "reversed")  

#Apply step 4 of algorithm to identify consecutive values that jump in size

#Function that identifies consecutive values that jump in size more than
#the maximum predicted amount that they could change

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

#Apply step 5 of algorithm to remove any remaining implausible values 

dat4 <- algo_cutoffs(dat4)


#remove values defined as outliers  
master_SZCO_A <- dat4 %>%
  filter(general_outlier == FALSE)



