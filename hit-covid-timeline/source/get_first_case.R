library(COVID19)
library(tidyverse)
library(googlesheets4)
library(ISOcodes)

# estimate first cases and deaths in the database
data <-world(type="country")
firstcase<- data %>% filter(confirmed>0) %>%
                group_by(country) %>% summarize(firstcase=min(date))
firstdeath<- data %>% filter(deaths>0) %>%
                group_by(country) %>% summarize(firstdeath=min(date))

#combine and add ISO country code
firsts <- full_join(firstcase,firstdeath) %>%
                left_join(select(ISO_3166_1,Name,Alpha_3),
                          by=c("country"="Name")) %>%
                mutate(
                       Alpha_3= ifelse(country=="Bolivia","BOL",Alpha_3),
                       Alpha_3= ifelse(country=="Brunei","BRN",Alpha_3),
                       Alpha_3= ifelse(country=="Burma","MMR",Alpha_3),
                       Alpha_3= ifelse(country=="Congo (Brazzaville)","COG",Alpha_3),
                       Alpha_3= ifelse(country=="Congo (Kinshasa)","COD",Alpha_3),
                       
                       Alpha_3= ifelse(country=="Cote d'Ivoire","CIV",Alpha_3),
                       Alpha_3= ifelse(country=="Iran","IRN",Alpha_3),
                       Alpha_3= ifelse(country=="Korea, South","KOR",Alpha_3),
                       #Alpha_3= ifelse(country=="Kosovo","KOS",Alpha_3),

                       
                       Alpha_3= ifelse(country=="Laos","LAO",Alpha_3),
                       Alpha_3= ifelse(country=="Moldova","MDA",Alpha_3),
                       Alpha_3= ifelse(country=="Russia","RUS",Alpha_3),
                       Alpha_3= ifelse(country=="Syria","SYR",Alpha_3),
                       
                       Alpha_3= ifelse(country=="Tanzania","TZA",Alpha_3),
                       Alpha_3= ifelse(country=="US","USA",Alpha_3),
                       Alpha_3= ifelse(country=="Venezuela","VEN",Alpha_3),
                       Alpha_3= ifelse(country=="Vietnam","VNM",Alpha_3)#,
                       #Alpha_3= ifelse(country=="West Bank and Gaza","MMR",Alpha_3)
                       
                       ) %>%
                filter(!is.na(Alpha_3)) %>%
                rename(ISOcode=Alpha_3)
                

#' writes data to google spreadsheet for website visualizations
#' @param dat data to write to spreadsheet
#' @param ss_id spreadsheet id (from google)
#' @param sheet_title title of sheet
write_to_gsheet <- function(dat,
                            ss_id = "1h0JA1b0gxqJcRTP6U740UpG-2t4S1HC3p-0IhIdjdog",
                            sheet_title="hit-covid-dashboard"
){
                
                cat("writing to google sheet \n")
                googlesheets4::sheets_write(dat,ss = "1h0JA1b0gxqJcRTP6U740UpG-2t4S1HC3p-0IhIdjdog",sheet="Sheet1")
}


#write to google sheets
write_to_gsheet(firsts)


