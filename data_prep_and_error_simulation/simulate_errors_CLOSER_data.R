#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script adds duplications and errors to the reformatted CLOSER
#data so that different data cleaning methods can be tested upon it to obtain 
#their comparative effectiveness

#LIBRARIES:
  library(tidyverse)
  library(Rcpp)

#read in data
  dat <- read_csv('../data/reformatted_CLOSER_data.csv')
  
#ADD DUPLICATIONS TO DATA

#Function that identifies duplications in the data and prints the number out
#Duplications are defined as a data entry for the same individual that was entered on
#the same day
#Complete duplications are defined as a data entry for the same individual that was entered on
#the same day and is the same value (in this case, weight measurement)
  get_duplications <- function(X, print_results = TRUE) {
          X <- X %>%
                  mutate(dups_ID = group_indices_(X, 
                                                  .dots=c("ind_ID", "age_floored")), 
                         complete_dups_ID = group_indices_(X, 
                                                           .dots=c("ind_ID", "age_floored", "new_weight")),
                         duplications = (duplicated(dups_ID) | 
                                                 duplicated(dups_ID, fromLast = TRUE)),
                         complete_duplications = (duplicated(complete_dups_ID) |
                                                          duplicated(complete_dups_ID, fromLast = TRUE)))
          if(print_results) {
                  print(c("Duplications", sum(X$duplications)))
                  print(c("Complete Duplications", sum(X$complete_duplications)))
          }
          return(X)
  }

#check data does not contain duplications
  dat <- get_duplications(dat) 

#randomly add duplications (of 2) to 2.5% of data
  d <- 0.025 #percentage of rows to duplicate (2.5%)
  set.seed(782)
  dups1 <- dat[sample(nrow(dat), (nrow(dat)*d)), ]
  dups1_expand <- dups1[rep(1:nrow(dups1),each=2),] 
  dat <- full_join(dat,dups1_expand)

#check duplications have been added
  dat <- get_duplications(dat) 

#Add further duplications (of 3) to 2.5% of this data
  set.seed(783)
  dups2 <- dat[sample(nrow(dat), (nrow(dat)*d)), ]
  dups2_expand <- dups2[rep(1:nrow(dups2),each=3),] 
  dat <- full_join(dat,dups2_expand)

#check duplications have been added
  dat <- get_duplications(dat) 

#create ID for each row in data
  dat$row_ID <- seq_len(nrow(dat))
  
#ADD ERRORS TO DATA
   
  if(!exists(c('e', 'r'))) {
#get e - the overall error rate
  e <- 0.01
#get r - the percentage of errors that IS a random error
  r <- 0.5
  }
  
  #get p - the percentage of errors that is NOT a random error (these are the fixed errors)
  p <- 1-r
  
#randomly select data that errors will be added to
  set.seed(1000)
  error_data <- dat[sample(nrow(dat), (nrow(dat)*e)), ]
  not_error_data <- subset(dat, !(dat$row_ID %in% error_data$row_ID))

#function that splits the error data into several smaller dataframes
  split_data <- function(dat, props = c(rep((p*100)/11, 11), (r*100)), which.adjust = 1){
          stopifnot(all(props >= 0), which.adjust <= length(props))
          props <- props/sum(props)
          n <- nrow(dat)
          ns <- round(n * props)
          ns[which.adjust] <- n - sum(ns[-which.adjust])
          ids <- rep(1:length(props), ns)
          which.group <- sample(ids)
          split(dat, which.group)
  }

  
  if(e == 0) {
    dat <- full_join(not_error_data,error_data) %>%
      mutate(weight = new_weight,
             induced_error = original_weight != weight)
    }
  
  if(e > 0) {
  
#Randomly split error data into 12 dataframes based on the proportions of random vs
#non-random errors given by 'r' and 'p' respectively
  set.seed(1001)
  split_error_data <- split_data(error_data)
  
  if(r == 0) {   
    make_random_errors <- error_data[FALSE,]
    }
  
  if(r > 0) {     
        
#Induce random errors
  make_random_errors <- split_error_data$`12`
  set.seed(1002)
  make_random_errors$weight <- sample(seq(0.0001, 500, by=0.0001), length(make_random_errors$ind_ID))
  make_random_errors$induced_random_errors <- TRUE
  error_data <- make_random_errors
  }
  
  if(p == 0) {
    error_data <- make_random_errors
  }
  
  if(p > 0) {  

#Induce multiplication errors (x10, x100 and x1000)
  multiply_10 <- split_error_data$`1`
  multiply_10$weight <- multiply_10$original_weight*10
  multiply_10$induced_div_10_errors <- TRUE
  error_data <- full_join(make_random_errors,multiply_10) 

  multiply_100 <- split_error_data$`2`
  multiply_100$weight <- multiply_100$original_weight*100
  multiply_100$induced_div_100_errors <- TRUE
  error_data <- full_join(error_data,multiply_100) 

  multiply_1000 <- split_error_data$`3`
  multiply_1000$weight <- multiply_1000$original_weight*1000
  multiply_1000$induced_div_1000_errors <- TRUE
  error_data <- full_join(error_data,multiply_1000) 

#Induce division errors (/10, /100 and /1000)
  divide_10 <- split_error_data$`4`
  divide_10$weight <- divide_10$original_weight/10
  divide_10$induced_mul_10_errors <- TRUE
  error_data <- full_join(error_data,divide_10) 
  
  divide_100 <- split_error_data$`5`
  divide_100$weight <- divide_100$original_weight/100
  divide_100$induced_mul_100_errors <- TRUE
  error_data <- full_join(error_data,divide_100) 
  
  divide_1000 <- split_error_data$`6`
  divide_1000$weight <- divide_1000$original_weight/1000
  divide_1000$induced_mul_1000_errors <- TRUE
  error_data <- full_join(error_data,divide_1000) 

#Induce addition errors (+100 and +1000)
  add_100 <- split_error_data$`7`
  add_100$weight <- add_100$original_weight+100
  add_100$induced_min_100_errors <- TRUE
  error_data <- full_join(error_data,add_100) 
  
  add_1000 <- split_error_data$`8`
  add_1000$weight <- add_1000$original_weight+1000
  add_1000$induced_min_1000_errors <- TRUE
  error_data <- full_join(error_data,add_1000) 

#Induce kg errors
  convert_to_kgs <- split_error_data$`9`
  convert_to_kgs$weight <- convert_to_kgs$original_weight*0.45359237
  convert_to_kgs$induced_lbs_errors <- TRUE
  error_data <- full_join(error_data,convert_to_kgs) 

#Induce lb errors
  convert_to_lbs <- split_error_data$`10`
  convert_to_lbs$weight <- convert_to_lbs$original_weight*2.2046226218
  convert_to_lbs$induced_kg_errors <- TRUE
  error_data <- full_join(error_data,convert_to_lbs)  

#Induce transformation errors
#cpp function that transforms numbers (changes the order of 2 digits e.g 26 to 62)
  cppFunction('IntegerVector Reverse_CPP2(IntegerVector dat) {
              int n = dat.size();
              IntegerVector out(n);
              IntegerVector datdat = clone(dat); 
              
              for (int i = 0; i < n; ++i){
                int reverse = 0;
                while(datdat[i] != 0) {
                  int remainder = datdat[i]%10;
                  reverse = reverse*10 + remainder;
                  datdat[i]/= 10;
                }
                out[i] = reverse;
              }
              
              return out;
              
  }')
  
  convert_to_tf <- split_error_data$`11`
  convert_to_tf$weight <- Reverse_CPP2(convert_to_tf$original_weight) + (convert_to_tf$original_weight %% 1)
  convert_to_tf$induced_reversed_errors <- convert_to_tf$original_weight != convert_to_tf$weight
  error_data <- full_join(error_data,convert_to_tf) 
  }
  
#join the error data back to the non-error data to form the full dataset and reformat
  dat <- full_join(not_error_data,error_data) %>%
          #get rid of NAS in weight variable by filling with original weight, then copy variable
          mutate(new_weight = case_when(is.na(weight) ~ original_weight,
                                    !is.na(weight) ~ weight),
                 weight = new_weight,
          #create variable that identifies induced errors
                 induced_error = original_weight != weight)
  }
  
#check that no further duplications have been added  
  dat <- get_duplications(dat)