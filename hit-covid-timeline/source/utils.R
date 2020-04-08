## utility functions for intervention tracker database


##' reloads packages and key function files
##' @return nothing
reload_source <- function(){
    if (!require('redcapAPI')) install.packages('redcapAPI'); library('redcapAPI')
    if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
    if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
    if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
    if (!require('readr')) install.packages('readr'); library('readr')
    if (!require('naniar')) install.packages('naniar'); library('naniar')
    if (!require('shiny')) install.packages('shiny'); library('shiny')
    if (!require('DT')) install.packages('DT'); library('DT')
    if (!require('markdown')) install.packages('markdown'); library('markdown')
    if (!require('stringr')) install.packages('stringr'); library('stringr')
    if (!require('ggiraph')) install.packages('ggiraph'); library('ggiraph')
    if (!require('sf')) install.packages('sf'); library('sf')
    if (!require('sp')) install.packages('sp'); library('sp')
    if (!require('leaflet')) install.packages('leaflet'); library('leaflet')
    if (!require('googlesheets4')) install.packages('googlesheets4'); library('googlesheets4')
    if (!require('googlesheets4')) devtools::install_github("tidyverse/googlesheets4"); library('googlesheets4') # using dev version
    

    #if (!require('geojsonio')) install.packages('geojsonio'); library('geojsonio')
    
    if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
    if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
    if (!require('knitr')) install.packages('knitr'); library('knitr')
    
    
    
    source("source/utils.R")
    ## load required packages not in reload_source()
}


##' pulls data from redcap database
##' be careful not to do this too often, it can really overload system
##' @param api_path path for file with api token
##' @param url_path path for file with redcap server url
##' @return returns data.frame of pulled data
pull_data <- function(api_path = "private/api_token.txt",
                      url_path = "private/url.txt"){

    ## pulling private stuff (not on git)
    api_token <- readLines(api_path)
    url <- readLines(url_path)

    ## Pulling data from Redcap
    rcon <- redcapConnection(url, api_token)
    data <- exportRecords(rcon, checkboxLabels = TRUE, labels = FALSE, factors = TRUE)
    
    #Reading the country in separately to get code while keeping factor (no factors)
    #while allowing factors for all other fields
    country_data <- exportRecords(rcon, fields = c("country", "record_id"), labels = FALSE, factors = FALSE)
    country_data <- country_data[, c("record_id", "country")]
    
    #Merging in country data
    data <- data %>% rename(country_name = country)
    data2 <- data %>% full_join(country_data, by = "record_id")

    return(data2)
}


#' Load long data by either pulling from API
#' and creating it on the fly or loading std csv 
#' file
#' @param fresh_pull generate directly from server?
#' @param long_file_path path to where the std long file is saved
#' csv file
get_long_data <- function(fresh_pull=FALSE,
                          long_file_path="generated_data/survey_data_long.csv",
                          ...){
    
    if(fresh_pull){
        cat(sprintf("pulling data from API \n"))
        source("source/utils.R")
        reload_source()
        source("source/data_cleaning_functions.R")
        data <- pull_data()
        
        # cleaning
        dataL_clean <- create_long(data, error_window = 2,...)
        
        cat(sprintf("Saving long file at %s \n",long_file_path))
        write.csv(dataL_clean, long_file_path, row.names = FALSE)
    } 
    
    rc <- tryCatch({
        read_csv(long_file_path,
                 col_types = cols(
            record_id = col_double(),
            geography_and_intro_timestamp = col_datetime(format = ""),
            admin_1_unit_and_updates_timestamp = col_datetime(format = ""),
            timestamp = col_datetime(format = ""),
            t_original = col_date(format = ""),
            size = col_double(),
            duration = col_double(),
            date_flag = col_logical(),
            date_error = col_logical(),
            t = col_date(format = "")
        )
        )
        },
        error=function(x){
         message(sprintf("file %s doesn't seem to exist \n",long_file_path))
            return(NA)
        }
    )

    return(rc)
}


#' takes national observations and propagates them
#' to all children for visuralization purposes
#' note: this does not do anything to specify if local policy trumps national policy
#' @param fresh_pull generate directly from server?
#' @param long_file_path path to where the std long file is saved
#' csv file
propagate_down_national <- function(long_data,
                                    lookup_loc="geo_lookup.csv"){
    
    geo <- read_csv(lookup_loc)
    
    nationals <- long_data %>% filter(national_entry=="Yes")
    
    ## filling out national entry to all children
    filled_out <- left_join(nationals,geo %>% rename(country=admin0)) %>% data.frame
    
    rc <- bind_rows(long_data %>% filter(national_entry=="No"),filled_out)
    
    return(rc)
}

#' writes data to google spreadsheet for website visualizations
#' @param dat data to write to spreadsheet
#' @param ss_id spreadsheet id (from google)
#' @param sheet_title title of sheet
#' @param propagate run propagate_down_national before posting
write_to_gsheet <- function(dat,
                            ss_id = "1SW6Q0x31tlLt-VroeVj21t-1z-byGT8hhMGBcjuL7U8",
                            sheet_title="hit-covid-dashboard",
                            propagate=TRUE
                            ){
    
    if(propagate){
        dat <- propagate_down_national(dat)
    }
    
    cat("writing to google sheet \n")
    googlesheets4::sheets_write(dat,ss = "1SW6Q0x31tlLt-VroeVj21t-1z-byGT8hhMGBcjuL7U8",sheet="latest")

}

