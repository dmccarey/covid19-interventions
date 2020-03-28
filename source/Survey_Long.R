#John's Hopkins COVID-19 Interventions Survey

setwd("~/Boston University/COVID_Interventions")

source("source/utils.R")
reload_source()

#Reading Data
url_path <- "~/Boston University/COVID_Interventions/url.txt"
api_path <- "~/Boston University/COVID_Interventions/api.txt"

data <- pull_data(api_path, url_path)


############################ Creating long version of dataset ##########################

# #Function to create long version
# create_long <- function(url, api_token){
#
#   #Pulling data from REDcap
#   rcon <- redcapConnection(url, api_token)
#   data <- exportRecords(rcon, checkboxLabels = FALSE, labels = FALSE, factors = FALSE)
#
#   #Move below into this function to read in raw data and create long version
# }


#Subsetting to rows with completed geography and admin1 (valid entries)
data2 <- (data
          %>% filter(geography_and_intro_complete == "Complete",
                         admin_1_unit_and_updates_complete == "Complete")
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
                     entertainment_closed_timestamp = leisure_and_entertainment_venue_closures_timestamp,
                     entertainment_closed_complete = leisure_and_entertainment_venue_closures_complete,
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
)
names(data2) <- gsub("updated_domains___", "", names(data2))
names(data2) <- gsub("tomatic_individuals", "", names(data2))
names(data2) <- gsub("closures", "closed", names(data2))
names(data2) <- gsub("desc", "details", names(data2))

#Columns that uniquely identify the record
idCols <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
            "redcap_survey_identifier", "geography_and_intro_timestamp",
            "email", "first_name", "last_name", "country", "country_name",
            "geography_and_intro_complete",
            "admin_1_unit_and_updates_timestamp", "adm1",
            "national_entry", "adm_lowest", "no_updates")


#### No updates ####

noupdates <- data2[data2$no_updates == 1, idCols]




#### Simple interventions ####

#Function to creating cleaned version of each simple intervention
clean_intervention <- function(interven_name){
  df <- data2[, c(idCols, names(data2)[grepl(interven_name, names(data2))])]
  df$intervention <- interven_name
  df$intervention_specific <- interven_name
  names(df) <- gsub(paste0(interven_name, "_"), "", names(df))
  return(df)
}

#Combining simple inverventions
interven_names <- c("limited_mvt", "household_confined", "nursing_home_closed",
                    "office_closed", "entertainment_closed", "store_closed",
                    "public_transport_closed", "public_space_closed", "social_group_limits",
                    "mask", "enforcement_deployed", "state_of_emergency")
interven_list <- NULL
for(i in 1:length(interven_names)){
  interven_list[[i]] <- clean_intervention(interven_names[i])
}
interven_df_simp <- bind_rows(interven_list)



#Names of complex interventions
#closed_border, symp_screening, school_closed, restaurant_closed, contact_tracing, testing_symp, testing_asymp, 

## Splitting closed_border into the specific interventions ##
border_df <- (data2[, c(idCols, names(data2)[grepl("border", names(data2))])]
               %>% rename(closed_border_in_air = closed_border_specific___1,
                          closed_border_in_land = closed_border_specific___2,
                          closed_border_in_sea = closed_border_specific___3,
                          closed_border_out_air = closed_border_specific___4,
                          closed_border_out_land = closed_border_specific___5,
                          closed_border_out_sea = closed_border_specific___6)
               %>% pivot_longer(cols = c("closed_border_in_air", "closed_border_in_land",
                                         "closed_border_in_sea", "closed_border_out_air",
                                         "closed_border_out_land", "closed_border_out_sea"),
                                names_to = "intervention_specific", values_to = "up_specific")
               %>% mutate(up_specific = ifelse(up_specific == "", "No Update", "Update"),
                          intervention = "closed_border")
)

#Column names that should be in all specific interventions
border_idCols <- c("intervention", "intervention_specific", "closed_border_up", "up_specific",
                   "closed_border_timestamp", "closed_border_source",
                   "closed_border_url", "closed_border_complete")

#Names of the specific interventions
border_names <- c("closed_border_in_air", "closed_border_in_land", "closed_border_in_sea",
                  "closed_border_out_air", "closed_border_out_land", "closed_border_out_sea")

#Loop through all of the specific interventions and combining
border_list <- NULL
for(i in 1:length(border_names)){
  df <- border_df[border_df$intervention_specific == border_names[i],
                     c(idCols, border_idCols, names(border_df)[grepl(border_names[i], names(border_df))])]
  names(df) <- gsub(paste0(border_names[i], "_"), "", names(df))
  names(df) <- gsub("closed_border_", "", names(df))
  border_list[[i]] <- df
}
border_dfL <- bind_rows(border_list)


#### Full intervention dataset ####

interven_df <- bind_rows(border_dfL, interven_df_simp)
interven_df <- (interven_df
                %>% mutate(up = ifelse(up == "", "No Update", "Update"))
                # %>% filter(interven_df, up == "Update",
                #            (up_specific == "Update" | is.na(up_specific)))
)

write.csv(interven_df, "survey_data_long.csv", row.names = FALSE)
