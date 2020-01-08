#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script runs all data cleaning methods 110 times over 
#different error rates and proportions of random errors to fixed errors and 
#calculates the sensitivity. specificity and data preservation of each method
#at each simulation

#LIBRARIES:
  library(tidyverse)
  library(evaluate)
  
#Main function that runs methods at different simulations, calculates sensitivity
#and specificity and monitors progress

  analyse_cleaning_method <- function(e, r) {
    
    #get time when function started
    fn_start_time <- Sys.time()
    
    #Progress analysis function
    get_progress <- function() {
    
    df_percentage <- df %>%
      mutate(percentage_complete = 0.9090909,
             percentage_complete = cumsum(percentage_complete))
    current_row <- which(df_percentage$e == e & df_percentage$r == r)
    
    percent_complete <- df_percentage[current_row,3]
    time_for_run <- difftime(Sys.time(),fn_start_time, units = "hours")
    time_total <- difftime(Sys.time(),start_time, units = "hours")
    
    print(paste('current error rate =', e))
    print(paste('current percentage of random errors = ', r, '%', sep =""))
    print(paste('current percentage of the way through = ', percent_complete, '%', sep = ""))
    print(paste('Time taken for this run =', time_for_run, "hours"))
    print(paste('Total time taken =', time_total, "hours"))
    print(paste('Estimated time remaining =', (time_total/percent_complete)*(100 - percent_complete), "hours"))
    }
    
    
    #Function to to get the data preservation, sensitivity and specificity of a method
  get_se_and_sp <- function(master_data, data_name, converge_fail = FALSE) {
      
      if(converge_fail == TRUE) {
        se <- NA
        sp <- NA
        pres <- NA}
      
      if(converge_fail == FALSE) {
        
        deletions <- subset(dat, !(dat$row_ID %in% master_data$row_ID)) 
        
        #percentage of data preserved
        pres <- ((length(master_data$ind_ID)/length(dat$ind_ID))*100)
        
        master_data <- master_data %>% 
          mutate(changed_weight = weight != new_weight)
        
        modifications <- subset(master_data, master_data$changed_weight == TRUE) 
        
        induced_errors <- subset(dat, dat$induced_error == TRUE)
        #Total number of errors
        te <- length(induced_errors$row_ID)
        #Total number of errors modified or deleted
        tec <- sum(deletions$induced_error == TRUE) +
          sum(modifications$induced_error == TRUE)
        
        #Sensitivity of method
        se <- tec/te*100
        
        not_induced_errors <- subset(dat, dat$induced_error == FALSE)
        #Total number of non-errors
        tne <- length(not_induced_errors$row_ID)
        #Total number of non-errors NOT corrected or deleted
        tnec <- tne - (sum(deletions$induced_error == FALSE & deletions$complete_duplications == FALSE) +
                         sum(modifications$induced_error == FALSE & modifications$complete_duplications == FALSE))
        
        #Specificity of method
        sp <- tnec/tne*100 
      }
      
      results <- data_frame(se,sp,pres)
      colnames(results) <- paste(colnames(results), data_name, sep = "_")
      
      return(results) 
      
    }
    
    #replay(evaluate) runs the code within a file and returnes the output as if it had
    #been run in the console. This is useful for workflows such as this where multiple files
    #are linked together and rely on eachother to work effectively
    
    #add duplications and errors to the data at the rates specified in the function
    #arguements (e and r)
    replay(evaluate(file('../data_prep_and_error_simulation/simulate_errors_CLOSER_data.R')))
    
    #RUN EACH METHOD
    replay(evaluate(file('../data_cleaning_methods/DNC.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/RD.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/GCO.R')))
    get_progress()
<<<<<<< HEAD
    replay(evaluate(file('../data_cleaning_methods/GCO_A.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/SZCO.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/SZCO_A.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/LZCO.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/LZCO_A.R')))
    get_progress()
=======
    replay(evaluate(file('../data_cleaning_methods/SZCO.R')))
    get_progress()
    replay(evaluate(file('../data_cleaning_methods/LZCO.R')))
    get_progress()
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    replay(evaluate(file('../data_cleaning_methods/NLR.R')))
    get_progress()
    
    #If the NLR model failed to converge, subsequent methods will not work so 
    #given NA data.frames
    if(exists('fem_mod') == FALSE | exists('mal_mod') == FALSE) {
      NLR_converge_fail <- TRUE
      master_NLR <- as.data.frame(NA)
      master_NLR_A <- as.data.frame(NA)
      master_NLME <- as.data.frame(NA)
      master_NLME_A <- as.data.frame(NA)
    }
    
    #If the NLR model did not fail to converge, run methods
    if(exists('fem_mod') == TRUE & exists('mal_mod') == TRUE) {
      NLR_converge_fail <- FALSE
      replay(evaluate(file('../data_cleaning_methods/NLR_A.R')))
      get_progress()
      replay(evaluate(file('../data_cleaning_methods/NLME.R')))
      get_progress()
      
    #If the NLME model failed to converge, subsequent methods will not work so 
    #given NA data.frames  
      if(exists('mixd') == FALSE) {
        NLME_converge_fail <- TRUE
        master_NLME <- as.data.frame(NA)
        master_NLME_A <- as.data.frame(NA)
      }  
    
    #If the NLME model did not fail to converge, run methods    
      if(exists('mixd') == TRUE) {
        NLME_converge_fail <- FALSE
        replay(evaluate(file('../data_cleaning_methods/NLME_A.R')))
        get_progress()
      }
    }
    
    
  #GET SENSITIVITY AND SPECIFICITY OF EACH METHOD
    master_DNC_res <- get_se_and_sp(master_DNC, "DNC")
    master_RD_res <- get_se_and_sp(master_RD, "RD")
    master_GCO_res <- get_se_and_sp(master_GCO, "GCO")
<<<<<<< HEAD
    master_GCO_A_res <- get_se_and_sp(master_GCO_A, "GCO_A")
    master_SZCO_res <- get_se_and_sp(master_SZCO, "SZCO")
    master_SZCO_A_res <- get_se_and_sp(master_SZCO_A, "SZCO_A")
    master_LZCO_res <- get_se_and_sp(master_LZCO, "LZCO")
    master_LZCO_A_res <- get_se_and_sp(master_LZCO_A, "LZCO_A")
=======
    master_SZCO_res <- get_se_and_sp(master_SZCO, "SZCO")
    master_LZCO_res <- get_se_and_sp(master_LZCO, "LZCO")
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    
    if(NLR_converge_fail == FALSE) {  
      master_NLR_res <- get_se_and_sp(master_NLR, "NLR")
      master_NLR_A_res <- get_se_and_sp(master_NLR_A, "NLR_A")
    }
    
    if(NLR_converge_fail == TRUE) {  
      master_NLR_res <- get_se_and_sp(master_NLR, "NLR", converge_fail = TRUE)
      master_NLR_A_res <- get_se_and_sp(master_NLR_A, "NLR_A", converge_fail = TRUE)
    }
    
    if(NLME_converge_fail == FALSE) {  
      master_NLME_res <- get_se_and_sp(master_NLME, "NLME")
      master_NLME_A_res <- get_se_and_sp(master_NLME_A, "NLME_A")
    }
    
    if(NLME_converge_fail == TRUE) {  
      master_NLME_res <- get_se_and_sp(master_NLME, "NLME", converge_fail = TRUE)
      master_NLME_A_res <- get_se_and_sp(master_NLME_A, "NLME_A", converge_fail = TRUE)
    }
    
    #Put all results in a dataframe
<<<<<<< HEAD
    all_results <- cbind(master_DNC_res, master_RD_res, master_GCO_res,master_GCO_A_res, master_SZCO_res, master_SZCO_A_res, 
                         master_LZCO_res, master_LZCO_A_res, master_NLR_res, master_NLR_A_res, master_NLME_res, master_NLME_A_res) 
=======
    all_results <- cbind(master_DNC_res, master_RD_res, master_GCO_res, master_SZCO_res, 
                         master_LZCO_res, master_NLR_res, master_NLR_A_res, master_NLME_res, master_NLME_A_res) 
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    
    #Run progress report final time
    get_progress()
    
    return(all_results)
  }


#This function runs the analyse cleaning methods function at the rates of e and r
#specified in a dataframe
  fun <- function(e, r) {
    results <- analyse_cleaning_method(e[1], r[1])
    return(results)
  }

#Run the setup file  
  replay(evaluate(file('../data_cleaning_methods/setup_file.R')))
  
#Create a dataframe wth the values of e and r required
<<<<<<< HEAD
  df <- expand.grid(e = c(0, 0.001, 0.002, 0.005,0.01,0.02,0.05,0.1,0.2, 0.5), r = c(seq(0, 1, 0.1)))
=======
  df <- expand.grid(e = c(0.01), r = c(0.5))
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192

#get start time when fun began iterating over e and r (for progress report)
  start_time <- Sys.time()
#run fun over each iteration of e and r specified in df
  methods_results <- df %>%
    bind_cols(., pmap_df(., fun))

  
<<<<<<< HEAD
  #UPDATE GIT
  system("git checkout update_growth_work")
  system("git status")		
  system("git add .")
  system("git commit -m 'update growth algorithm stuff for paper with multi sim running over weekend'")
  system("git push")
  
  
=======
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
#Change NaN senstivity results to 100   

  #function to deal with NaNs in dataframes
  is.nan.data.frame <- function(x){
    do.call(cbind, lapply(x, is.nan))
  }
  
  methods_results[is.nan(methods_results)] <- 100
    
#write results to a csv
<<<<<<< HEAD
  write_csv(methods_results, '../data/multi_simulation_results3.csv')
=======
  write_csv(methods_results, '../data/multi_simulation_results.csv')
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192




  
  