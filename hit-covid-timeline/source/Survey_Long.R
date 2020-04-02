#Script for pulling data and making a nicer dataset for visuralization and analyses


# Where to save clean data file
out_file_path <- "generated_data/survey_data_long.csv"

# Sourcing utils and cleaning functions
source("source/utils.R")
reload_source()
source("source/data_cleaning_functions.R")

# Reading raw survey data
cat(sprintf("pulling data from API \n"))
data <- pull_data()

# Reading in the country/admin1 lookup table and subsetting to just admin1 info
admin_lookup <- read_csv("geo_lookup.csv")
admin_lookup2 <- (admin_lookup
                 %>% select(adm1 = GID_1, admin1_name = NAME_1)
                 %>% filter(!is.na(adm1))
)


# Columns that uniquely identify a record
idCols <- c("record_id","geography_and_intro_timestamp",
            "email", "first_name", "last_name", "country", "country_name",
            "geography_and_intro_complete",
            "admin_1_unit_and_updates_timestamp", "adm1", "admin1_name",
            "national_entry", "adm_lowest", "no_updates")

# Names of simple inverventions
interven_names_simp <- c("limited_mvt", "household_confined", "nursing_home_closed",
                         "office_closed", "entertainment_closed", "store_closed",
                         "public_transport_closed", "public_space_closed", "social_group_limits",
                         "testing_symp", "testing_asymp", "mask", "enforcement_deployed",
                         "state_of_emergency")



#### Running Cleaning/Long Function ####

dataL_clean2 <- create_long(data, idCols, interven_names_simp, error_window = 2)

cat(sprintf("Saving long file at %s \n",out_file_path))
write.csv(dataL_clean2, out_file_path, row.names = FALSE)



