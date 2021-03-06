

find_interven_info <- function(admin_tab, intervention){
  
  one_int <- admin_tab %>%
    filter(intervention_clean == intervention) %>%
    arrange(intervention_specific_clean, date_of_update) %>%
    select(-country_name, -admin1_name, -national_entry, -intervention_clean, -date_flag, -status_flag)
    
    # Removing empty columns except status and date_of_update
    keepCols <- unique(c(one_int %>% select_if(~ !any(is.na(.))) %>% names(),
                         "status", "date_of_update"))
    one_int <- one_int[, keepCols]
    
    
    if("subpopulation" %in% names(one_int)){
      p <- ggplot(data = one_int, aes(x = date_of_update, y = intervention_specific_clean,
                                      color = status_simp, alpha = subpop_plot)) +
        scale_alpha_manual(values=c('Entire Population' = 1, 'Not Entire Population' = .4)) +
        scale_color_manual(values=c('Strongly Implemented'="red3",'Partially Implemented'=
                                      "gold1", 'Not Implemented'= "green4")) +
        scale_x_date(limits = c(min(long_data$t_original, na.rm = TRUE), today() + 2)) +
        geom_point(size = 3) +
        theme_bw() +
        theme(legend.position="bottom",
              legend.box = "vertical",
              plot.title = element_text(size=18, hjust = 0.5)) + 
        labs(y = "",
             x = "Date of Policy Change",
             color="Policy Status",
             alpha="Subpopulation") +
        guides(color = guide_legend(order = 1),
               alpha = guide_legend(order = 2))
    } else{
      p <- ggplot(data = one_int, aes(x = date_of_update, y = intervention_specific_clean,
                                      color = status_simp)) +
        scale_color_manual(values=c('Strongly Implemented'="red",'Partially Implemented'=
                                      "yellow", 'Not Implemented'= "green")) +
        scale_x_date(limits = c(min(long_data$t_original, na.rm = TRUE), today())) +
        geom_point(size = 3) +
        theme_bw() +
        theme(legend.position="bottom",
              plot.title = element_text(size=18, hjust = 0.5)) + 
        labs(y = "",
             x = "Date of Policy Change",
             color="Policy Status")
    }
  
  print_tab <- one_int %>% select(-status_simp, -subpop_plot, -duplicate_flag, -row_id)
  hideCol <- which(names(print_tab) == "any_flag")
  
  df <- datatable(print_tab,
                  class = "display nowrap compact", # style
                  options = list(
                    scrollX = TRUE, # allow user to scroll wide tables horizontally
                    pageLength = 10,
                    columnDefs = list(list(targets = hideCol, visible = FALSE))
                  )
  ) %>%
    formatStyle(
      columns = "any_flag",
      target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', 'yellow'))
    )
  
return(list(p, df))
}
