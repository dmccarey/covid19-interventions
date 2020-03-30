#Script for pulling data and making a nicer dataset for visuralization and analyses


# Where to save clean data file
out_file_path <- "generated_data/survey_data_long.csv"

#Reading data
source("hit-covid-timeline/source/utils.R")
reload_source()

#Reading raw survey data
cat(sprintf("pulling data from API \n"))
data <- pull_data()

#Columns that uniquely identify a record
idCols <- c("record_id","geography_and_intro_timestamp",
            "email", "first_name", "last_name", "country", "country_name",
            "geography_and_intro_complete",
            "admin_1_unit_and_updates_timestamp", "adm1",
            "national_entry", "adm_lowest", "no_updates")

#Names of simple inverventions
interven_names_simp <- c("limited_mvt", "household_confined", "nursing_home_closed",
                         "office_closed", "entertainment_closed", "store_closed",
                         "public_transport_closed", "public_space_closed", "social_group_limits",
                         "testing_symp", "testing_asymp", "mask", "enforcement_deployed",
                         "state_of_emergency")



#### Functions to Clean the Simple Interventions ####

#Function to create cleaned version of each simple intervention
clean_interven_simp <- function(survey_data, idCols, interven_name){
  
  #Subsetting to columns related to this intervention
  interven_df <- survey_data[, c(idCols, names(survey_data)[grepl(interven_name, names(survey_data))])]
  #Adding intervention names
  interven_df$intervention <- interven_name
  interven_df$intervention_specific <- interven_name
  #Creating general column names
  names(interven_df) <- gsub(paste0(interven_name, "_"), "", names(interven_df))
  
  return(interven_df)
}

#Function to create long version of all simple interventions
combine_interven_simp <- function(survey_data, idCols, interven_names){
  
  #Initializing list
  interven_list <- NULL
  
  #Looping through interventions and cleaning column names
  for(i in 1:length(interven_names)){
    interven_list[[i]] <- clean_interven_simp(survey_data, idCols, interven_names[i])
  }
  #Row binding intervention dataframes
  interven_dfL <- bind_rows(interven_list)
  
  return(interven_dfL)
}



#### Functions to Clean the Complex Interventions ####

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

#closed_border
closed_border_long <- function(survey_data, idCols, interven_name){
  
  
  #Names of the specific interventions
  specific_names <- c("closed_border_in_air", "closed_border_in_land", "closed_border_in_sea",
                      "closed_border_out_air", "closed_border_out_land", "closed_border_out_sea")
  
  #Renaming columns and creating inital long version with specific interventions on different rows
  interven_df <- (survey_data[, c(idCols, names(survey_data)[grepl(interven_name, names(survey_data))])]
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

#symp_screening
symp_screening_long <- function(survey_data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("symp_screening_air", "symp_screening_land",
                      "symp_screening_sea", "symp_screening_within")
  
  #Subsetting and renaming columns
  interven_df <- (survey_data[, c(idCols, names(survey_data)[grepl(interven_name, names(survey_data))])]
                  %>% rename(symp_screening_air = symp_screening_specific___1,
                             symp_screening_land = symp_screening_specific___2,
                             symp_screening_sea = symp_screening_specific___3,
                             symp_screening_within = symp_screening_specific___4)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#school_closed
school_closed_long <- function(survey_data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("nursery_school_closed", "primary_school_closed",
                      "sec_school_closed", "postsec_school_closed",
                      "unknown_school_closed")
  
  #Subsetting and renaming columns
  interven_df <- (survey_data[, c(idCols, names(survey_data)[grepl(interven_name, names(survey_data))])]
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

#restaruant_closed
restaurant_closed_long <- function(survey_data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("restaurant_closed", "restaurant_reduced")
  
  #Subsetting and renaming columns
  interven_df <- (survey_data[, c(idCols, names(survey_data)[grepl("restaurant", names(survey_data))])]
                  %>% rename(restaurant_reduced = restaurant_reduced_up,
                             restaurant_reduced_size = restaurant_reduced_max)
                  %>% mutate(restaurant_closed = restaurant_closed_up)
  )
  
  #Running function to clean and combine specific interventions
  interven_dfL <- combine_interven_comp(interven_df, idCols, specific_names, interven_name)
  
  return(interven_dfL)
}

#contact_tracing
contact_tracing_long <- function(survey_data, idCols, interven_name){
  
  #Names of the specific interventions
  specific_names <- c("contact_tracing", "contact_quarantine")
  
  #Subsetting and renaming columns
  interven_df <- (survey_data[, c(idCols, names(survey_data)[grepl(interven_name, names(survey_data))])]
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



#### Function to Create Long Version of Dataset ####
#Function to create full long version
create_long <- function(raw_survey_data, idCols){

  
  #### Intial cleaning ####
  
  #Subsetting to rows with completed geography and admin1 (valid entries)
  #Renaming columns as needed and doing other preliminary cleaning
  data2 <- (raw_survey_data
            %>% filter(geography_and_intro_complete == "Complete",
                       admin_1_unit_and_updates_complete == "Complete")
            #Changing all factors to character
            %>% mutate_if(is.factor, as.character)
            #Renaming REDCap variables to match naming conventions
            %>% rename(closed_border_timestamp = border_closures_timestamp,
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
            %>% mutate(updated_domains___no_updates = ifelse(updated_domains___no_updates == "", FALSE, TRUE))
            #Uniting the testing for symptomatic and asymptomatic patients
            %>% replace_with_na(list(testing_symp_elg___1 = "",
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
                                     testing_asymp_elg___6 = ""))
            %>% unite(testing_symp_pop, testing_symp_elg___1, testing_symp_elg___2,
                      testing_symp_elg___3, testing_symp_elg___4, testing_symp_elg___5,
                      testing_symp_elg___6, testing_symp_elg___7, testing_symp_elg___8,
                      sep = ";", na.rm = TRUE, remove = TRUE)
            %>% unite(testing_asymp_pop, testing_asymp_elg___1, testing_asymp_elg___2,
                      testing_asymp_elg___3, testing_asymp_elg___4, testing_asymp_elg___5,
                      testing_asymp_elg___6, sep = ";", na.rm = TRUE, remove = TRUE)
            %>% replace_with_na(list(testing_symp_pop = "",
                                     testing_asymp_pop = ""))
  )
  names(data2) <- gsub("updated_domains___", "", names(data2))
  names(data2) <- gsub("tomatic_individuals", "", names(data2))
  names(data2) <- gsub("closures", "closed", names(data2))
  names(data2) <- gsub("desc", "details", names(data2))
  
  
  #### Creating long versions of each intervention ####
  
  #Subsetting to rows with no updates at all
  no_updates <- data2[data2$no_updates == TRUE, idCols]
  #Subsetting to rows with some updates
  some_updates <- data2[data2$no_updates == FALSE, ]
  
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
  
  
  #### Final cleaning ####
  
  #Final cleaning including
  interven_dfL_clean <- (interven_dfL
                         %>% mutate(up = ifelse(up == "", "No Update", "Update"),
                                    required = ifelse(is.na(required) & req == "yes", "required",
                                               ifelse(is.na(required) & req == "no", "recommended",
                                    required)))
                         #Removing interventions that are incomplete no update
                         %>% filter(up == "Update" &
                                      (up_specific == "Update" | is.na(up_specific)),
                                    complete == "Complete")
                         #Not adding rows which intentionally denote no update for now
                         #%>% bind_rows(no_updates)
                         %>% replace_na(list(national_entry = "No"))
                         #Removing names and emails and req (combined with required above)
                         %>% select(-first_name, -last_name, -email, -req, -no_updates,
                                    -geography_and_intro_complete)
  )

}

#### Running Cleaning/Long Function ####

interven_dfL_clean <- create_long(data, idCols)

cat(sprintf("Saving long file at %s \n",out_file_path))
write.csv(interven_dfL_clean, out_file_path, row.names = FALSE)
