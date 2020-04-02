#Script for pulling data and making a nicer dataset for visuralization and analyses


# Where to save clean data file
out_file_path <- "generated_data/survey_data_long.csv"

#Reading data
source("source/utils.R")
reload_source()

#Reading raw survey data
cat(sprintf("pulling data from API \n"))
data <- pull_data()

#Reading in the country/admin1 lookup table and subsetting to just admin1 info
admin_lookup <- read_csv("geo_lookup.csv")
admin_lookup2 <- (admin_lookup
                 %>% select(adm1 = GID_1, admin1_name = NAME_1)
                 %>% filter(!is.na(adm1))
)


#Columns that uniquely identify a record
idCols <- c("record_id","geography_and_intro_timestamp",
            "email", "first_name", "last_name", "country", "country_name",
            "geography_and_intro_complete",
            "admin_1_unit_and_updates_timestamp", "adm1", "admin1_name",
            "national_entry", "adm_lowest", "no_updates")

#Names of simple inverventions
interven_names_simp <- c("limited_mvt", "household_confined", "nursing_home_closed",
                         "office_closed", "entertainment_closed", "store_closed",
                         "public_transport_closed", "public_space_closed", "social_group_limits",
                         "testing_symp", "testing_asymp", "mask", "enforcement_deployed",
                         "state_of_emergency")




#### Data cleaning functions ####

#Funtion to rename columns to match naming scheme
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
           enforcement_deployed_complete = military_and_police_deployment_complete)
  
  names(data2) <- gsub("updated_domains___", "", names(data2))
  names(data2) <- gsub("tomatic_individuals", "", names(data2))
  names(data2) <- gsub("closures", "closed", names(data2))
  names(data2) <- gsub("desc", "details", names(data2))
  
  return(data2)
}

#Funtion unite the testing criteria for symptomatic and asymptomatic patients
unite_testing <- function(data){
  
  data %>%
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
    
    unite(testing_symp_pop, testing_symp_elg___1, testing_symp_elg___2,
          testing_symp_elg___3, testing_symp_elg___4, testing_symp_elg___5,
          testing_symp_elg___6, testing_symp_elg___7, testing_symp_elg___8,
          sep = ";", na.rm = TRUE, remove = TRUE) %>%
    
    unite(testing_asymp_pop, testing_asymp_elg___1, testing_asymp_elg___2,
          testing_asymp_elg___3, testing_asymp_elg___4, testing_asymp_elg___5,
          testing_asymp_elg___6, sep = ";", na.rm = TRUE, remove = TRUE) %>%
    
    replace_with_na(list(testing_symp_pop = "",
                         testing_asymp_pop = ""))
}

#' Function to clean dates
#' 
#' @param dataL Long version of dataset with column name 't' and 'timestamp'
#' @param error_window How many days ahead of the date of entry is the intervention date
#' considered an error (default is 2)
#' @return data with t replaced with clean date and t_original with original date
#' and returns a column called date_flag which is TRUE if the intervention date
#' is after the date of entry and date_error if the intervention date is in
#' the same month 
clean_dates <- function(dataL, error_window = 2){
  
  dataL2 <- dataL %>%
    rename(t_original = t) %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
           t_new = as.character(make_date(year = year(t_original),
                                          month = month(t_original) - 1,
                                          day = day(t_original))),
           t_old = as.character(t_original),
           #date_flag = TRUE if the data entry date is after the intervention date
           date_flag = t_original > timestamp,
           #date_error = TRUE if the entry date and intervention date are in the same month
           #and the intervention date is in the the future by more than 2 days 
           date_error = t_original > timestamp &
             month(t_original) == month(timestamp) &
             day(t_original) - day(timestamp) > error_window) %>%
    mutate(t = as.POSIXct(ifelse(!is.na(date_flag) & date_flag == TRUE,
                                 t_new, t_old))) %>%
    select(-t_old, -t_new)
  
  return(dataL2)
}



#### Functions create long versions all interventions ####

#Function to create cleaned version of each simple intervention
clean_interven_simp <- function(data, idCols, interven_name){
  
  #Subsetting to columns related to this intervention
  interven_df <- survey_data[, c(idCols, names(data)[grepl(interven_name, names(data))])]
  #Adding intervention names
  interven_df$intervention <- interven_name
  interven_df$intervention_specific <- interven_name
  #Creating general column names
  names(interven_df) <- gsub(paste0(interven_name, "_"), "", names(interven_df))
  
  return(interven_df)
}

#Function to create long version of all simple interventions
combine_interven_simp <- function(data, idCols, interven_names){
  
  #Initializing list
  interven_list <- NULL
  
  #Looping through interventions and cleaning column names
  for(i in 1:length(interven_names)){
    interven_list[[i]] <- clean_interven_simp(data, idCols, interven_names[i])
  }
  #Row binding intervention dataframes
  interven_dfL <- bind_rows(interven_list)
  
  return(interven_dfL)
}

#Function to create long version of the complex interventions
combine_interven_comp <- function(interven_df, idCols, specific_names, interven_name){
  
  #Creating inital long version with specific interventions on different rows
  interven_df2 <- (interven_df
                   %>% pivot_longer(cols = specific_names,
                                    names_to = "intervention_specific", values_to = "up_specific")
                   %>% mutate(up_specific = ifelse(up_specific == "", "No Update", "Update"),
                              intervention = interven_name)
  )
  
  suffixes <- c("timestamp", "source", "url", "complete")
  interven_idCols <- c("intervention", "intervention_specific",
                       paste0(interven_name, "_up"), "up_specific",
                       paste(interven_name, suffixes, sep = "_"))
  
  #Loop through all of the specific interventions
  #Subsetting to relevant columns and creating genetic column names
  interven_list <- NULL
  for(i in 1:length(specific_names)){
    df <- interven_df2[interven_df2$intervention_specific == specific_names[i],
                       c(idCols, interven_idCols, names(interven_df2)[grepl(specific_names[i],
                                                                            names(interven_df2))])]
    names(df) <- gsub(paste0(specific_names[i], "_"), "", names(df))
    names(df) <- gsub(paste0(interven_name, "_"), "", names(df))
    interven_list[[i]] <- df
  }
  #Combining individual data frames
  interven_dfL <- bind_rows(interven_list)
  
  return(interven_dfL)
}

#Function to create cleaned version of closed_border
closed_border_long <- function(data, idCols, interven_name){
  
  
  #Names of the specific interventions
  specific_names <- c("closed_border_in_air", "closed_border_in_land", "closed_border_in_sea",
                      "closed_border_out_air", "closed_border_out_land", "closed_border_out_sea")
  
  #Renaming columns and creating inital long version with specific interventions on different rows
  interven_df <- (survey_data[, c(idCols, names(data)[grepl(interven_name, names(data))])]
                  %>% rename(closed_border_in_air = closed_border_specific___1,
                             closed_border_in_land = closed_border_specific___2,
                             closed_border_in_sea = closed_border_specific___3,
                             closed_border_out_air = closed_border_specific___4,
                             closed_border_out_land = closed_border_specific___5,
                             closed_border_out_sea = closed_border_specific___6)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#Function to create cleaned version of symp_screening
symp_screening_long <- function(data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("symp_screening_air", "symp_screening_land",
                      "symp_screening_sea", "symp_screening_within")
  
  #Subsetting and renaming columns
  interven_df <- (data[, c(idCols, names(data)[grepl(interven_name, names(data))])]
                  %>% rename(symp_screening_air = symp_screening_specific___1,
                             symp_screening_land = symp_screening_specific___2,
                             symp_screening_sea = symp_screening_specific___3,
                             symp_screening_within = symp_screening_specific___4)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#Function to create cleaned version of school_closed
school_closed_long <- function(data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("nursery_school_closed", "primary_school_closed",
                      "sec_school_closed", "postsec_school_closed",
                      "unknown_school_closed")
  
  #Subsetting and renaming columns
  interven_df <- (data[, c(idCols, names(data)[grepl(interven_name, names(data))])]
                  %>% rename(nursery_school_closed = school_closed_specific___1,
                             primary_school_closed = school_closed_specific___2,
                             sec_school_closed = school_closed_specific___3,
                             postsec_school_closed = school_closed_specific___4,
                             unknown_school_closed = school_closed_specific___9)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#Function to create cleaned version of restaruant_closed
restaurant_closed_long <- function(data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("restaurant_closed", "restaurant_reduced")
  
  #Subsetting and renaming columns
  interven_df <- (data[, c(idCols, names(data)[grepl("restaurant", names(data))])]
                  %>% rename(restaurant_reduced = restaurant_reduced_up,
                             restaurant_reduced_size = restaurant_reduced_max)
                  %>% mutate(restaurant_closed = restaurant_closed_up)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#Function to create cleaned version of contact_tracing
contact_tracing_long <- function(data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("contact_tracing", "contact_quarantine")
  
  #Subsetting and renaming columns
  interven_df <- (data[, c(idCols, names(data)[grepl(interven_name, names(data))])]
                  %>% mutate(contact_tracing = contact_tracing_up,
                             contact_quarantine_status = contact_tracing_quarantine)
                  %>% rename(contact_quarantine_t = contact_tracing_quarantine_t,
                             contact_quarantine_details = contact_tracing_quarantine_details,
                             contact_quarantine = contact_tracing_quarantine)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#Function to combine all of the individual long datasets into one
combine_interven <- function(data){
  
  #Subsetting to rows with some updates
  some_updates <- data[data$no_updates == FALSE, ]
  
  #Creating long version for simple interventions
  interven_df_simp <- combine_interven_simp(some_updates, idCols, interven_names_simp)
  #Creating long version for complex interventions
  border_dfL <- closed_border_long(some_updates, idCols, "closed_border")
  screening_dfL <- symp_screening_long(some_updates, idCols, "symp_screening")
  restaurant_dfL <- restaurant_closed_long(some_updates, idCols, "restaurant_closed")
  contact_dfL <- contact_tracing_long(some_updates, idCols, "contact_tracing")
  
  #Combining all of the intervention datasets 
  interven_dfL <- bind_rows(border_dfL,
                            interven_df_simp,
                            screening_dfL,
                            restaurant_dfL,
                            contact_dfL)
  
  return(interven_dfL)
}





#### Function to Create Long Version of Dataset ####

create_long <- function(data, idCols){
  
  #Subsetting to rows with completed geography and admin1 (valid entries)
  data2 <- data  %>%
    filter(geography_and_intro_complete == "Complete",
           admin_1_unit_and_updates_complete == "Complete") %>%
    #Changing all factors to character
    mutate_if(is.factor, as.character) %>%
    #Merging in the admin1 names
    left_join(admin_lookup2, by = "adm1") %>%
    #Creating an indicator if the entry has no updates
    mutate(updated_domains___no_updates = ifelse(updated_domains___no_updates == "", FALSE, TRUE))
  
  #Renaming REDCap variables to match naming conventions
  data3 <- rename_cols(data2)
  
  #Uniting the testing criteria for symptomatic and asymptomatic patients
  data4 <- unite_testing(data3)

  #Creating long version of the survey data
  dataL <- combine_interven(data4)

  
  
  #### Final cleaning ####
  
  #Subsetting to rows with no updates at all
  no_updates <- data2[data2$no_updates == TRUE, idCols]
  
  #Final cleaning including
  interven_dfL_clean <- (interven_dfL
                         %>% mutate(up = ifelse(up == "", "No Update", "Update"),
                                    required = ifelse(is.na(required) & req == "yes", "required",
                                               ifelse(is.na(required) & req == "no", "recommended",
                                    required)),
                                    #Removing non-unicode characters
                                    details = gsub("[^[:alnum:][:blank:]?&/\\-\\.\\,\\;]", "", details),
                                    details = gsub("\\\xe9", "e", details),
                                    record_id = as.numeric(record_id))
                         #Removing interventions that are incomplete no update
                         %>% filter(up == "Update" &
                                      (up_specific == "Update" | is.na(up_specific)),
                                    complete == "Complete")
                         #Not adding rows which intentionally denote no update for now
                         #%>% bind_rows(no_updates)
                         %>% replace_na(list(national_entry = "No"))
                         #Removing names and emails and req (combined with required above)
                         #%>% select(-first_name, -last_name, -email, -req, -no_updates,
                         #           -geography_and_intro_complete)
                         # cleaning up country names a little bit 
  ) %>% mutate(country_name = str_remove_all(country_name,"[^[:alnum:] ]"))
  
  #Correcting dates (original dates in column t_original)
  interven_dfL_clean2 <- clean_dates(interven_dfL_clean)
}


#### Running Cleaning/Long Function ####

interven_dfL_clean <- create_long(data, idCols)

cat(sprintf("Saving long file at %s \n",out_file_path))
write.csv(interven_dfL_clean, out_file_path, row.names = FALSE)
