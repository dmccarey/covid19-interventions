#John's Hopkins COVID-19 Interventions Survey

setwd("~/Boston University/COVID_Interventions")
library(redcapAPI)
library(dplyr)
library(tidyr)
library(ggplot2)

## Pulling data from Redcap
rcon <- redcapConnection(url, api_token)
data <- exportRecords(rcon, checkboxLabels = TRUE, labels = FALSE, factors = FALSE)


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
          %>% replace_na(list(national_entry = "No"))
          #REMOVE WHEN GET NEW NAMES#
          %>% rename(household_confined_timestamp = household_confinement_timestamp,
                     household_confined_complete = household_confinement_complete,
                     nursing_home_closed_timestamp = nursing_homelongterm_care_closures_timestamp,
                     nursing_home_closed_complete = nursing_homelongterm_care_closures_complete,
                     closed_border_timestamp = border_closures_timestamp,
                     closed_border_complete = border_closures_complete)
)
names(data2) <- gsub("updated_domains___", "", names(data2))

#Columns that uniquely identify the record
idCols <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
            "redcap_survey_identifier", "geography_and_intro_timestamp",
            "email", "first_name", "last_name", "country",
            "geography_and_intro_complete",
            "admin_1_unit_and_updates_timestamp", "adm1",
            "national_entry", "adm_lowest")



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
interven_names <- c("nursing_home_closed", "household_confined")
interven_list <- NULL
for(i in 1:length(interven_names)){
  interven_list[[i]] <- clean_intervention(interven_names[i])
}
interven_df_simp <- bind_rows(interven_list)



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
                %>% mutate(up = ifelse(up == 0, "No Update","Update"))
                # %>% filter(interven_df, up == "Update",
                #            (up_specific == "Update" | is.na(up_specific)))
)

write.csv(interven_df, "survey_data_long.csv", row.names = FALSE)



