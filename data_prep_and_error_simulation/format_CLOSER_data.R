#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script formats the CLOSER data so that necessary variables
#are created, unneccessary varibles are removed and data is in the correct class
#for the subsequent processes applied to the data

#LIBRARIES:
  library(tidyverse)

#read in original CLOSER data (for reference see README.MD repository file)
  dat <- read_tsv('../data/original_CLOSER_data.tab')

#reformat and organise the data
  dat <- dat %>%
    #rename variables
    rename(ind_ID = CLOSERID, 
           original_weight = wt) %>%
    #remove NAs in the weight data 
    filter(!is.na(original_weight)) %>% 
    #reformat sex, study ID, unit and measurement type to be more explicit/universally understood
    mutate(sex = case_when(sex == '1' ~ 'Male',
                           sex == '2' ~ 'Female'),
           study_ID = case_when(stid == '1' ~ 'nshd',
                                stid == '2' ~ 'ncds',
                                stid == '3' ~ 'bcs70',
                                stid == '4' ~ 'alspac',
                                stid == '5' ~ 'mcs'),
           unit = case_when(wtimp == '1' ~ 'Metric',
                            wtimp == '2' ~ 'Imperial'),
           measurement_type = case_when(wtself == '1' ~ 'Measured',
                                        wtself == '2' ~ 'Self-reported'),
    #transform age into days and create new binary age variable
           age = round(as.numeric(xage*365), 4),
           age_floored = floor(age),
    #reformat ID and sex into appropriate class
           ind_ID = as.factor(ind_ID),
           sex = as.factor(sex), 
    #create copy of weight variable
<<<<<<< HEAD
           new_weight = as.numeric(original_weight),
    row_ID = row_number()) %>%
=======
           new_weight = as.numeric(original_weight)) %>%
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
    #find the number of data entries per individual
    group_by(ind_ID) %>%
    mutate(num_data_entries = length(ind_ID)) %>%
    ungroup() %>%
    #select only the variables that are needed for this study
<<<<<<< HEAD
    select(ind_ID, row_ID, study_ID, sex, age, age_floored, original_weight, 
=======
    select(ind_ID, study_ID, sex, age, age_floored, original_weight, 
>>>>>>> a36da5f7209fd77e8f32b63d5d80b476c72bc192
           new_weight, num_data_entries, unit, measurement_type)

#Save the data in csv format
  write_csv(dat, '../data/reformatted_CLOSER_data.csv')
