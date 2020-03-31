## utility functions for intervention tracker database


##' reloads packages and key function files
##' @return nothing
reload_source <- function(){
    library(pacman)
    p_load(redcapAPI,dplyr,tidyr,ggplot2,readr,naniar,shiny,purrr,DT,markdown,leaflet,stringr,ggiraph)
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
                          long_file_path="generated_data/survey_data_long.csv"){
    
    if(fresh_pull){
        source("source/Survey_Long.R")
    } 
    
    rc <- tryCatch({
        read_csv(long_file_path)
        },
        error=function(x){
         message(sprintf("file %s doesn't seem to exist \n",long_file_path))
            return(NA)
        }
    )

    return(rc)
}