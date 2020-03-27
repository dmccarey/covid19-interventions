#John's Hopkins COVID-19 Interventions Survey

setwd("~/Boston University/COVID_Interventions")
library(dplyr)
library(tidyr)
library(ggplot2)

## Reading in the long dataset
interven_df <- read.csv("survey_data_long.csv")


######################## Table and Plot of Updates ###########################

#Clean table of just updates
interven_df_clean <- (interven_df
                      %>% filter(interven_df, up == "Update",
                                 (up_specific == "Update" | is.na(up_specific)),
                                 complete == "Complete")
                      %>% select(record_id, entry_time = geography_and_intro_timestamp,
                                 national_entry, country, admin1 = adm1,
                                 locality = adm_lowest, intervention, intervention_specific,
                                 up, up_specific, status, subpopulation = pop,
                                 date_updated = t, required, enforcement, details)
)

#Plot of updates
ggplot(data = interven_df_clean %>% filter(admin1 == "USA.1_1" |
                                             (national_entry == "Yes" &
                                                country == "United States of America")),
       aes(x = date_updated, y = intervention_specific,
           shape = status, color = national_entry)) +
  geom_point(size = 3)

#Need to figure out how to have country information apply to states


