## generate all reports
sep_country_folders <- FALSE

## load data to see what admin units have data
source("source/utils.R")
source("source/data_report_functions.R")
reload_source()
interven_names <- read_csv("intervention_lookup.csv")
long_data <- get_long_data(fresh_pull = TRUE, long_file_path = "generated_data/survey_data_long.csv", remove_names=TRUE)
last_updated_time <- file.info("generated_data/survey_data_long.csv")$mtime
unique_locs <- long_data %>% 
  arrange(country, adm1) %>%
  select(country, adm1) %>%
  distinct

date <- Sys.Date()
out_dir <- paste0("audit_reports/",date,"/")
if(!dir.exists(out_dir)){
  dir.create(out_dir)
}

## loop through and make reports
for (i in 1:nrow(unique_locs)){
  
  cat(sprintf("making report for %s (%s) \n",unique_locs[i,2],unique_locs[i,1]))
  
  tryCatch({
    
    
    if(sep_country_folders){
      cntry_folder <- paste0(out_dir,unique_locs$country[i])
      
      if(!dir.exists(cntry_folder)){
        dir.create(cntry_folder)
      }

            out_file_name <- paste0(cntry_folder,"/",paste(unique_locs[i,],collapse="-"),"_",date,".html")
      
    } else {
            out_file_name <- paste0(out_dir,paste(unique_locs[i,],collapse="-"),"_",date,".html")
    }
    tmp_params = list(national_input=ifelse(is.na(unique_locs[i,2]),TRUE,FALSE),
                      country_input =ifelse(is.na(unique_locs[i,2]),unique_locs[i,1],""),
                      admin_input = ifelse(is.na(unique_locs[i,2]),"",unique_locs[i,2]))
    rmarkdown::render("source/data_report.Rmd",
                      output_file = paste0("../",out_file_name),
                      params = tmp_params,
                      quiet = TRUE)  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

