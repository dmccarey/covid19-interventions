#John's Hopkins COVID-19 Interventions Survey

setwd("~/Boston University/COVID_Interventions")
library(dplyr)
library(tidyr)
library(ggplot2)

## Reading in the long dataset
interven_dfL_clean <- read.csv("survey_data_long.csv")


######################## Table and Plot of Updates ###########################


## ROUGH VERSION ##

#Clean table of all updates
interven_df_table <- (interven_dfL_clean
                      %>% filter(complete == "Complete")
                      %>% select(record_id, entry_time = geography_and_intro_timestamp,
                                 national_entry, country, admin1 = adm1,
                                 locality = adm_lowest, intervention, intervention_specific,
                                 status, subpopulation = pop, date_of_update = t,
                                 required, enforcement, details)
                      %>% mutate(status = ifelse(status %in% c("closed", "fully closed",
                                                               "fully restricted", "all",
                                                               "required", "yes"), 3,
                                          ifelse(status %in% c("partially closed", "partially restricted",
                                                               "recommended", "some"), 2,
                                          ifelse(status %in% c("open", "no", "no policy"), 1, status))),
                                 status = factor(status, levels = c(1, 2, 3),
                                                 labels = c("open/no/no policy",
                                                            "partially closed/partially restricted/\nrecommended/some",
                                                            "closed/restricted/all/yes")))
)

#Plot of all updates
ggplot(data = interven_df_table,
       aes(x = date_of_update, y = intervention_specific,
           shape = status, color = national_entry)) +
  geom_point(size = 3)


## CREATE FUNCTION TO MAKE TABLE AND PLOT BY ADMIN1 UNIT ##


