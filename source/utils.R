## utility functions for intervention tracker database


##' reloads packages and key function files
##' @return nothing
reload_source <- function(){
    library(redcapAPI)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    source("source/utils.R")
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
    data <- exportRecords(rcon, checkboxLabels = TRUE, labels = FALSE, factors = FALSE)

    return(data)
}
