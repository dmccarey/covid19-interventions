#Functions used to clean the dataset for visuralization, analyses, and sharing

#### Data cleaning functions ####

#' Funtion to rename columns to match naming scheme
#' @param data wide version of survey data
#' @returns original dataframe with columns renamed
rename_cols <- function(data){
  data2 <- data %>% 
    rename(closed_border_timestamp = border_closures_timestamp,
           closed_border_complete = border_closures_complete,
           limited_mvt_timestamp = limiting_movement_within_administrative_unit_borde_timestamp,
           limited_mvt_complete = limiting_movement_within_administrative_unit_borde_complete,
           household_confined_timestamp = household_confinement_timestamp,
           household_confined_complete = household_confinement_complete,
           symp_screening_timestamp = symptom_screening_timestamp,
           symp_screening_complete = symptom_screening_complete,
           nursing_home_closed_timestamp = nursing_homelongterm_care_closures_timestamp,
           nursing_home_closed_complete = nursing_homelongterm_care_closures_complete,
           entertainment_closed_timestamp = leisure_entertainment_and_religious_venue_closures_timestamp,
           entertainment_closed_complete = leisure_entertainment_and_religious_venue_closures_complete,
           restaurant_closed_timestamp = restaurant_closures_and_restrictions_timestamp,
           restaurant_closed_complete = restaurant_closures_and_restrictions_complete,
           store_closed_timestamp = retail_store_closures_timestamp,
           store_closed_complete = retail_store_closures_complete,
           public_transport_closed_timestamp = public_transportation_closures_timestamp,
           public_transport_closed_complete = public_transportation_closures_complete,
           social_group_limits_timestamp = limiting_gatherings_timestamp,
           social_group_limits_complete = limiting_gatherings_complete,
           mask_timestamp = universal_face_mask_policies_timestamp,
           mask_complete = universal_face_mask_policies_complete,
           enforcement_deployed_timestamp = military_and_police_deployment_timestamp,
           enforcement_deployed_complete = military_and_police_deployment_complete,
           iso_details = isolation_desc,
           quar_iso_timestamp = quarantine_and_homeisolation_timestamp,
           quar_iso_complete = quarantine_and_homeisolation_complete)
  
  names(data2) <- gsub("updated_domains___", "", names(data2))
  names(data2) <- gsub("tomatic_individuals", "", names(data2))
  names(data2) <- gsub("closures", "closed", names(data2))
  names(data2) <- gsub("desc", "details", names(data2))
  
  return(data2)
}


#' Funtion unite the testing criteria for symptomatic and asymptomatic patients
#' @param data wide version of survey data
#' @returns original dataframe with testing conditions for symptomatic and asymptomatic
#' individuals collapsed as one string into the 'pop' column
unite_testing <- function(data){
  
  data2 <- data %>%
    # Replacing "" with NA
    replace_with_na(list(testing_symp_elg___1 = "",
                         testing_symp_elg___2 = "",
                         testing_symp_elg___3 = "",
                         testing_symp_elg___4 = "",
                         testing_symp_elg___5 = "",
                         testing_symp_elg___6 = "",
                         testing_symp_elg___7 = "",
                         testing_symp_elg___8 = "",
                         testing_asymp_elg___1 = "",
                         testing_asymp_elg___2 = "",
                         testing_asymp_elg___3 = "",
                         testing_asymp_elg___4 = "",
                         testing_asymp_elg___5 = "",
                         testing_asymp_elg___6 = "")) %>%
    
    # Combining the symptomatic conditions under the 'pop' column
    unite(testing_symp_test_pop, testing_symp_elg___1, testing_symp_elg___2,
          testing_symp_elg___3, testing_symp_elg___4, testing_symp_elg___5,
          testing_symp_elg___6, testing_symp_elg___7, testing_symp_elg___8,
          sep = ";", na.rm = TRUE, remove = TRUE) %>%
    
    # Combining the asymptomatic conditions under the 'pop' column
    unite(testing_asymp_test_pop, testing_asymp_elg___1, testing_asymp_elg___2,
          testing_asymp_elg___3, testing_asymp_elg___4, testing_asymp_elg___5,
          testing_asymp_elg___6, sep = ";", na.rm = TRUE, remove = TRUE) %>%
    
    # Again replacing "" with NA
    replace_with_na(list(testing_symp_test_pop = "",
                         testing_asymp_test_pop = ""))
  
  return(data2)
}


#' Function to clean dates 
#' @param dataL long version of dataset with column name 't' and 'timestamp'
#' @param error_window number days ahead of the date of entry is the intervention date
#' and represents a calendar entry error so should be recoded to the month before (default is 2)
#' @return long dataset with the following changes:
#'     A column called date_flag which is TRUE if the intervention date
#'     is after the date of entry
#'     A column called date_error if the intervention date is in the same month
#'     and more than the the error_window number of days after the date of entry.
#'     A column called t_original which maintains the original date (value of t)
#'     The column t replaced with the new date - corrected to the month before if
#'     date_error = TRUE
clean_dates <- function(dataL, error_window = 2){
  
  dataL2 <- dataL %>%
    
    # Retaining old t as t_original
    rename(t_original = t) %>%
    
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
           # Making a new date which is the month before (incorrect calendar entry)
           t_new = as.character(make_date(year = year(t_original),
                                          month = month(t_original) - 1,
                                          day = day(t_original))),
           t_old = as.character(t_original),
           # date_flag = TRUE if the data entry date is after the intervention date
           date_flag = t_original > timestamp,
           # date_error = TRUE if the entry date and intervention date are in the same month
           # and the intervention date is in the the future by more than 2 days 
           date_error = t_original > timestamp &
             month(t_original) == month(timestamp) &
             day(t_original) - day(timestamp) > error_window) %>%
    
    # Correcting t if date_error = TRUE
    mutate(t = as.POSIXct(ifelse(!is.na(date_error) & date_error == TRUE,
                                 t_new, t_old))) %>%
    select(-t_old, -t_new)
  
  return(dataL2)
}



#### Functions create long versions all interventions ####

#' Function to create cleaned version of each simple intervention
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @param interven_name string representing the name of the intervention
#' (should be prefix of column names)
#' @return dataframe with just the info about the intervention denoted by interven_name
#' with the columns renamed removing the intervention pre-fix
clean_interven_simp <- function(data, idCols, interven_name){
  
  # Subsetting to columns related to this intervention
  interven_df <- data[, c(idCols, names(data)[grepl(interven_name, names(data))])]
  # Adding intervention names
  interven_df$intervention <- interven_name
  interven_df$intervention_specific <- interven_name
  # Creating general column names
  names(interven_df) <- gsub(paste0(interven_name, "_"), "", names(interven_df))
  
  return(interven_df)
}


#' Function to create long version of all simple interventions
#' Runs clean_interven_simp() for all of the simple interventions
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @param interven_names_simp vector of the names of the simple interventions
#' (should be prefix of the column names)
#' @return a long dataframe with the output of clean_interven_simp() row binded
#' for all of the simple interventions
combine_interven_simp <- function(data, idCols, interven_names_simp){
  
  # Initializing list
  interven_list <- NULL
  
  # Looping through interventions and cleaning column names
  for(i in 1:length(interven_names_simp)){
    interven_list[[i]] <- clean_interven_simp(data, idCols, interven_names_simp[i])
  }
  # Row binding intervention dataframes
  interven_dfL <- bind_rows(interven_list)
  
  return(interven_dfL)
}


#' Function to create long version of the complex interventions
#' It is run by all of the intervention specific functions which make long versions
#' of the complex interventions
#' @param interven_df dataframe with the information from one complex intervention
#' @param idCols vector of column names that uniquely identify an entry
#' @param specific_names vector of names of specific interventions that make up the complex
#' interventions (should be prefix of the column names)
#' @param interven_name string representing the name of the complex intervention
#' (should be prefix of update column)
#' @return a long dataframe with just the info about the intervention denoted by interven_name
#' with the columns renamed removing the intervention pre-fix and combining all specific
#' interventions represented by the specific_names.
combine_interven_comp <- function(interven_df, idCols, specific_names, interven_name){
  
  # Creating inital long version with specific interventions on different rows
  interven_df2 <- interven_df %>%
    pivot_longer(cols = specific_names,
                 names_to = "intervention_specific",
                 values_to = "up_specific") %>%
    mutate(up_specific = ifelse(up_specific == "", "No Update", "Update"),
           intervention = interven_name)
  
  suffixes <- c("timestamp", "source", "url", "complete")
  interven_idCols <- c("intervention", "intervention_specific",
                       paste0(interven_name, "_up"), "up_specific",
                       paste(interven_name, suffixes, sep = "_"))
  
  # Loop through all of the specific interventions
  # Subsetting to relevant columns and creating genetic column names
  interven_list <- NULL
  for(i in 1:length(specific_names)){
    df <- interven_df2[interven_df2$intervention_specific == specific_names[i],
                       c(idCols, interven_idCols, names(interven_df2)[grepl(specific_names[i],
                                                                            names(interven_df2))])]
    names(df) <- gsub(paste0(specific_names[i], "_"), "", names(df))
    names(df) <- gsub(paste0(interven_name, "_"), "", names(df))
    interven_list[[i]] <- df
  }
  # Combining individual data frames
  interven_dfL <- bind_rows(interven_list)
  
  return(interven_dfL)
}



#' Function to create cleaned version of closed_border; runs combine_interven_comp()
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @return a long dataframe for the specific interventions grouped into closed_border
closed_border_long <- function(data, idCols){
  
  interven_name <- "closed_border"
  
  # Names of the specific interventions
  specific_names <- c("closed_border_in_air", "closed_border_in_land", "closed_border_in_sea",
                      "closed_border_out_air", "closed_border_out_land", "closed_border_out_sea")
  
  # Renaming columns and creating inital long version with specific interventions on different rows
  interven_df <- data[, c(idCols, names(data)[grepl(interven_name, names(data))])] %>%
    rename(closed_border_in_air = closed_border_specific___1,
           closed_border_in_land = closed_border_specific___2,
           closed_border_in_sea = closed_border_specific___3,
           closed_border_out_air = closed_border_specific___4,
           closed_border_out_land = closed_border_specific___5,
           closed_border_out_sea = closed_border_specific___6)
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#' Function to create cleaned version of symp_screening; runs combine_interven_comp()
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @return a long dataframe for the specific interventions grouped into symp_screening
symp_screening_long <- function(data, idCols){
  
  interven_name <- "symp_screening"
  
  # Names of the specific interventions
  specific_names <- c("symp_screening_air", "symp_screening_land",
                      "symp_screening_sea", "symp_screening_within")
  
  # Subsetting and renaming columns
  interven_df <- data[, c(idCols, names(data)[grepl(interven_name, names(data))])] %>%
    rename(symp_screening_air = symp_screening_specific___1,
           symp_screening_land = symp_screening_specific___2,
           symp_screening_sea = symp_screening_specific___3,
           symp_screening_within = symp_screening_specific___4)
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#' Function to create cleaned version of school_closed; runs combine_interven_comp()
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @return a long dataframe for the specific interventions grouped into school_closed
school_closed_long <- function(data, idCols){
  
  interven_name <- "school_closed"
  
  # Names of the specific interventions
  specific_names <- c("nursery_school_closed", "primary_school_closed",
                      "sec_school_closed", "post_school_closed",
                      "unknown_school_closed")
  
  #Subsetting and renaming columns
  interven_df <- data[, c(idCols, names(data)[grepl(interven_name, names(data))])] %>%
    rename(nursery_school_closed = school_closed_specific___1,
           primary_school_closed = school_closed_specific___2,
           sec_school_closed = school_closed_specific___3,
           post_school_closed = school_closed_specific___4,
           unknown_school_closed = school_closed_specific___9,
           post_school_closed_status = postsec_school_closed_status,
           post_school_closed_pop = postsec_school_closed_pop,
           post_school_closed_req = postsec_school_closed_req,
           post_school_closed_t = postsec_school_closed_t,
           post_school_closed_details = postsec_school_closed_details)
  
  # Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  # Making status unknown for unknown_school_closed
  interven_dfL <-interven_dfL %>%
    mutate(status = ifelse(intervention_specific == "unknown_school_closed",
                                 "unknown", status))
  
  return(interven_dfL)
}

#' Function to create cleaned version of restaurant_closed; runs combine_interven_comp()
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @return a long dataframe for the specific interventions grouped into restaurant_closed
restaurant_closed_long <- function(data, idCols){
  
  interven_name <- "restaurant_closed"
  
  # Names of the specific interventions
  specific_names <- c("restaurant_closed", "restaurant_reduced")
  
  # Subsetting and renaming columns
  interven_df <- data[, c(idCols, names(data)[grepl("restaurant", names(data))])] %>%
    rename(restaurant_reduced = restaurant_reduced_up,
           restaurant_reduced_size = restaurant_reduced_max) %>%
    mutate(restaurant_closed = restaurant_closed_up,
           restaurant_reduced = ifelse(restaurant_reduced == "no", "",
                                       restaurant_reduced))
  
  # Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#' Function to create cleaned version of quar_iso; runs combine_interven_comp()
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @return a long dataframe for the specific interventions grouped into quar_iso
quar_iso_long <- function(data, idCols){
  
  interven_name <- "quar_iso"
  
  # Names of the specific interventions
  specific_names <- c("quar_travel", "quar_confirm", "quar_suspect", "quar_other",
                      "iso_suspect", "iso_confirm", "iso_discharged")
  
  # Subsetting and renaming columns
  interven_df <- data[, c(idCols, names(data)[grepl("quar_|iso_", names(data))])] %>%
    rename(quar_travel = quar_pop___1,
           quar_confirm = quar_pop___2,
           quar_suspect = quar_pop___3,
           quar_other = quar_pop___4,
           iso_suspect = iso_pop___1,
           iso_confirm = iso_pop___2,
           iso_discharged = iso_pop___3)
  
  # Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}



#' Function to combine all of the individual long datasets into one
#' Runs combine_interven_simp() and all of the complex intervention functions
#' and then row binds the information for all interventions to create a long dataframe
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @param interven_names_simp vector of the names of the simple interventions
#' (should be prefix of the column names)
#' @return long version of data combining all interventions with generic column names
combine_interven <- function(data, idCols, interven_names_simp){
  
  # Creating long version for simple interventions
  interven_df_simp <- combine_interven_simp(data, idCols, interven_names_simp)
  
  # Creating long version for complex interventions
  border_dfL <- closed_border_long(data, idCols)
  school_dfL <- school_closed_long(data, idCols)
  screening_dfL <- symp_screening_long(data, idCols)
  restaurant_dfL <- restaurant_closed_long(data, idCols)
  quar_dfL <- quar_iso_long(data, idCols)
  
  # Combining all of the intervention datasets 
  dataL <- bind_rows(border_dfL,
                     school_dfL,
                     interven_df_simp,
                     screening_dfL,
                     restaurant_dfL,
                     quar_dfL)
  return(dataL)
}


#### Creating Cleaned Long Version of Dataset ####

#' Function to create cleaned long version of the dataset
#' @param data wide verison of survey data
#' @param idCols vector of column names that uniquely identify an entry
#' @param interven_names_simp vector of the names of the simple interventions
#' (should be prefix of the column names) 
#' @param error_window number days ahead of the date of entry is the intervention date
#' and represents a calendar entry error so should be recoded to the month before (default is 2)
#' @param remove_names flag for whether we remove data enty people's names and emails
#' @return a long version of the survey data with only the rows that represent updates to the
#' interventions and removing names and emails for public use
create_long <- function(data, 
                        idCols=c("record_id","geography_and_intro_timestamp",
                                 "email", "first_name", "last_name", "country", "country_name",
                                 "geography_and_intro_complete",
                                 "admin_1_unit_and_updates_timestamp", "adm1", "admin1_name",
                                 "national_entry", "adm_lowest", "no_updates"), 
                        interven_names_simp=c("limited_mvt", "household_confined", "nursing_home_closed",
                                              "office_closed", "entertainment_closed", "store_closed",
                                              "public_transport_closed", "public_space_closed", "social_group_limits",
                                              "contact_tracing", "testing_symp", "testing_asymp", "mask", 
                                              "enforcement_deployed", "state_of_emergency"),
                        error_window = 2, 
                        remove_names=TRUE){
  
  ## Intial Cleaning ##
  
  # Reading in the country/admin1 lookup table and subsetting to just admin1 info
  admin_lookup <- read_csv("geo_lookup.csv")
  admin_lookup2 <- admin_lookup %>% 
                   select(adm1 = GID_1, admin1_name = NAME_1) %>%
                   filter(!is.na(adm1))
  
  # Subsetting to rows with completed geography and admin1 (valid entries)
  data2 <- data  %>%
    filter(geography_and_intro_complete == "Complete",
           admin_1_unit_and_updates_complete == "Complete") %>%
    # Changing all factors to character
    mutate_if(is.factor, as.character) %>%
    # Merging in the admin1 names
    left_join(admin_lookup2, by = "adm1") %>%
    # Creating an indicator if the entry has no updates
    mutate(updated_domains___no_updates = ifelse(updated_domains___no_updates == "", FALSE, TRUE))
  
  # Renaming REDCap variables to match naming conventions
  data3 <- rename_cols(data2)
  
  # Uniting the testing criteria for symptomatic and asymptomatic patients
  data4 <- unite_testing(data3)
  
  
  ## Creating long version of dataset ##
  
  # Subsetting to rows with some updates
  some_updates <- data4[data4$no_updates == FALSE, ]
  # Creating long version of the survey data
  dataL <- combine_interven(some_updates, idCols, interven_names_simp)
  
  # #Subsetting to rows with no updates at all
  # #Note right now this is not included in the dataset becaue probably not of interest
  # #But can be added in the future if desired
  # no_updates <- data4[data4$no_updates == TRUE, idCols]
  
  
  ## Final cleaning ##
  
  dataL_clean <- dataL %>%
    
    mutate(record_id = as.numeric(record_id),
           # Recoding update
           up = ifelse(up == "", "No Update", "Update"),
           # Combining req and required (same question) and adding information from status
           required = ifelse((!is.na(req) & req == "yes") | 
                               (!is.na(status) & status == "required"), "required",
                      ifelse((!is.na(req) & req == "no") |
                               (!is.na(status) & status == "recommended"), "recommended",
                      ifelse((!is.na(req) & req == "unknown"), "unknown",
                                    required))),
           # Removing non-unicode characters
           details = gsub("[^[:alnum:][:blank:]?&/\\-\\.\\,\\;]", "", details),
           details = gsub("\\\xe9", "e", details),
           # cleaning up country names a little bit 
           country_name = str_remove_all(country_name,"[^[:alnum:] ]")) %>%
    replace_na(list(national_entry = "No")) %>%
    
    # Removing interventions that are incomplete: no update
    # Also removing fake entries
    filter(up == "Update" &
             (up_specific == "Update" | is.na(up_specific)),
           complete == "Complete",
           !grepl("fake", email)) %>%
    
    # Removing columns that aren't of interest
    select(-req, -no_updates,
           -geography_and_intro_complete) %>%
    
    # Arranging by record_id
    arrange(record_id)
  
  if(remove_names){
    dataL_clean <- dataL_clean %>% mutate(data_entry_by = paste0(toupper(substr(first_name,1,1)), ".",
                                                                 toupper(substr(last_name,1,1)),".")) %>% 
      replace_with_na(list(data_entry_by = "NA.NA.")) %>%
      select(-first_name, -last_name, -email)
  }
  
  # Correcting dates (original dates in column t_original)
  dataL_clean2 <- clean_dates(dataL_clean, error_window = error_window)
  
  return(dataL_clean2)
}

