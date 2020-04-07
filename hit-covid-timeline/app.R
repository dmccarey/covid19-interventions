## load functions and source packages
library(shinydashboard)
source("source/utils.R")
reload_source()

## some look table for countries and admin units
admin_lookup <- read_csv("geo_lookup.csv")
country_names <- setNames(admin_lookup$admin0,nm =admin_lookup$NAME_0 ) #%>% na.omit
admin_names <- setNames(admin_lookup$GID_1,nm =admin_lookup$NAME_1 ) %>% na.omit

## look-up table for clean intervention names
interven_names <- read_csv("intervention_lookup.csv")

## set up data
long_data <- get_long_data(fresh_pull = FALSE,long_file_path = "generated_data/survey_data_long.csv",remove_names=TRUE)
last_updated_time <- file.info("generated_data/survey_data_long.csv")$mtime

interven_df_plot <- long_data   %>% 
    filter(complete == "Complete") %>% 
    #Adding cleaned intervention names
    left_join(interven_names, by = c("intervention_specific", "intervention")) %>%
    select(record_id, entry_time = geography_and_intro_timestamp,
           national_entry, country, country_name, admin1 = adm1, admin1_name,
           locality = adm_lowest, intervention_specific_clean,
           date_of_update = t, status, subpopulation = pop,
           required, enforcement, size, duration, test_pop, details) %>%
    mutate(status_simp = ifelse(status %in% c("all",
                                              "closed",
                                              "complete contact tracing",
                                              "fully restricted",
                                              "required",
                                              "yes"), 1,
                                ifelse(status %in% c("partial contact tracing",
                                                     "partially closed",
                                                     "partially restricted",
                                                     "recommended",
                                                     "some"), 2,
                                       ifelse(status %in% c("open",
                                                            "no",
                                                            "no policy",
                                                            "none",
                                                            "unrestricted"), 3, status))),
           status_simp = factor(status_simp, levels = c(1, 2, 3),
                                labels = c("Strongly Implemented",
                                           "Partially Implemented",
                                           "Not Implemented")),
           # Making new requirement metric
           required_new = ifelse(intervention_specific_clean %in% c("Limiting size of gatherings",
                                                       "Symptom screening when entering by land")
                             & is.na(required), "unknown",
                             ifelse(is.na(required), "required", required)),
           intervention_f = factor(intervention_specific_clean))


# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(title = "HIT-COVID Data Viewer"),
    dashboardSidebar(            
        #h3("Choose a location:", style = sprintf("color:%s", "steelblue")),
                                 selectInput("country_select",label = "Select a country:",choices = country_names,selectize = TRUE),
                                 selectInput("admin_unit",label = "Select an admin1 unit:",choices = admin_names,selectize = TRUE),
                                 checkboxInput("include_national", label ="Include National Interventions?", value = TRUE, width = NULL),
                                 br(),
                                 downloadButton("download_data", label = "Download Current Data")
    ),
    dashboardBody(
        tabsetPanel(
            tabPanel("Overview",
                     br(),
                     #h3("Background"),
                     #h4("As the COVID-19 pandemic unfolds, massive government efforts are being made globally to try to reduce morbidity and mortality. Governments have taken a large range of actions, from broad-scale social distancing such as the forced lockdown of cities with mandatory home confinement, to behavior change campaigns to improve hand hygiene. Moreover, governments have implemented these measures at different points in time during the course of their epidemic."),
                     #h4("Major government mandated actions come with huge economic risks and many are asking if some of the most drastic actions are worth it. In order to start to understand how different public health policy interventions may have influenced COVID-19 transmission across the globe, we need detailed data on when and where specific policy interventions have been enacted over the course of this epidemic. The goal of this project is to provide a comprehensive database of public health policy at the first level administrative unit, to serve as a key component of assessments of the impact of these policies on COVID transmission dynamics and other changes in the health of affected populations. This living database will be maintained throughout the course of the pandemic with visual summaries of raw data made available publicly. "),
                     #h3("Progress"),
                     h4(sprintf("last update: %s" ,last_updated_time)),
                     h4(sprintf("%.0f interventions logged",nrow(interven_df_plot))),
                     h4(sprintf("%.0f countries covered",n_distinct(interven_df_plot$country))),
                     br(),
                     leafletOutput("simp_map"),
                     h4("Figure. Overview of recent updates")
                     #plotOutput('recordHeatmap')
            ),
            tabPanel("Timeline",
                     br(),
                     girafeOutput("timeline"),
                     p("Below is a table of all the data to explore:"),
                     dataTableOutput("overview_tab"),
                     includeMarkdown("include/heading_box.md"),
            )
        ))
    )


# Define server logic required to draw a histogram
server <- function(input, output,session) {

    
    output$download_data <- downloadHandler(
        filename = function() {
            paste("hit-covid-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write_csv(interven_df_plot, file)
        }
    )
    
    #heatmap to see what records were added
    output$recordHeatmap <- renderPlot({
        
        hmdf <-interven_df_plot  %>% group_by(date=floor_date(entry_time,unit="days")) %>%
            summarize(records=n()) %>%
            mutate(wd=wday(date,label=TRUE,abbr=TRUE),
                   week=floor_date(date,unit="weeks")) 
        
        
        hmdf %>% ggplot(aes(x=week,y=wd))+geom_tile(aes(fill=records))+
            scale_fill_distiller("Records" ,
                                 palette = "Blues",
                                 direction = 1, 
                                 na.value = "grey")+
            theme_bw() + theme(axis.title = element_blank())
        
    })
    
    

    country_select <- reactive({
        filter(admin_lookup, admin0 == input$country_select)
    })
    
    observeEvent(country_select(), {
        tmp<-country_select() #%>% filter(GID_1 %in% interven_df_plot$admin1) # limitng to those where we have data
        choices <- setNames(tmp$GID_1,nm=tmp$NAME_1) #%>% na.omit
        updateSelectInput(session,"admin_unit", choices = choices) 
    })
    
    # spreadsheet of data to scroll through
    output$overview_tab <- renderDT(
        interven_df_plot   %>% 
            #include only the admin1 level area selected
            filter(admin1 %in% input$admin_unit) %>%
            #include national interventions if selected
            rbind({
                if(input$include_national){
                    filter(interven_df_plot,
                           country == input$country_select &
                               is.na(admin1))
                } else data.frame() #empty data.frame
            }) %>%
            select(-national_entry, -status_simp, -required_new,
                   -country, -admin1, intervention_f) %>%
            arrange(admin1_name),
        class = "display nowrap compact", # style
        filter = "top", # location of column filters,
        options = list(  # options
            scrollX = TRUE # allow user to scroll wide tables horizontally
        )
        
    )
    
    
    #old overview tab april 6, 2020
    # output$overview_tab <- renderDT(
    #     interven_df_plot   %>% 
    #         filter(country == input$country_select) %>%
    #         select(-national_entry, -status_simp, -required_new,
    #                -country, -admin1, intervention_f) %>%
    #         arrange(admin1_name),
    #     class = "display nowrap compact", # style
    #     filter = "top", # location of column filters,
    #     options = list(  # options
    #         scrollX = TRUE # allow user to scroll wide tables horizontally
    #     )
    #     
    # )
    
    ## make timeline plot
    output$timeline <- renderGirafe({
        
        tmp <- interven_df_plot
        
        if(input$include_national){
            # figure out which country
            # get national level data and create fake obs for this admin unit
            cntry <- admin_lookup %>% dplyr::filter(GID_1==input$admin_unit) %>% select(admin0) %>% unlist %>% first
            
            national_ints <- tmp %>% dplyr::filter(national_entry=="Yes" & country==cntry) %>% 
                mutate(admin1 = input$admin_unit)
            
            tmp<-bind_rows(tmp,national_ints)
            # get national observations
        }
        
        
        ## creating tool tips for hover
        ## can make this nicer later
        tmp <- tmp %>%
            mutate(tooltip=paste0("record id: ",record_id,"\n",
                                  "date: ",date_of_update,"\n",
                                  "intervention: ",intervention_specific_clean,"\n",
                                  "status: ",status,"\n",
                                  "subpopulation: ",subpopulation,"\n",
                                  "required: ",required,"\n",
                                  "enforcement: ",enforcement,"\n",
                                  "size: ", size)) %>%
            filter(admin1 %in% input$admin_unit) %>%
            mutate(subpopulation = ifelse(subpopulation == "entire population", 
                                          "Entire Population",
                                          ifelse(!is.na(subpopulation), "Not Entire Population",
                                                 NA)))
        
        gg <- ggplot(data = tmp,
                     aes(x = date_of_update, y = intervention_f,
                         color = status_simp, shape = national_entry,
                         alpha = subpopulation)) +
            scale_y_discrete(drop=FALSE) +
            scale_alpha_manual(values=c('Entire Population' = 1, 'Not Entire Population' = .4)) +
            scale_color_manual(values=c('Strongly Implemented'="red",'Partially Implemented'=
                                            "yellow", 'Not Implemented'= "green")) +
            geom_point_interactive(aes(tooltip = tooltip, data_id = record_id), size = 8) +
            theme_bw() +
            theme(legend.position="bottom",
                  legend.box = "vertical") + 
            labs(y = "",
                 x = "Date of Policy Change",
                 color="Policy Status",
                 shape="National Policy?",
                 alpha="Subpopulation") +
            guides(color = guide_legend(order = 1),
                   alpha = guide_legend(order = 2),
                   shape = guide_legend(order = 3)) +
        theme(text = element_text(size=20))#, axis.text.x = element_text(angle=90)) +
        #scale_x_discrete(position = "top") +
        #scale_y_reverse(labels = function(x) as_date(x))
        
        girafe(ggobj = gg, width_svg = 20, height_svg =20)

    })
    
    ## making simple map
    
    output$simp_map <- renderLeaflet({

        #Last updated world map
        simp_dat2 <- interven_df_plot %>% 
            group_by(country) %>% 
            summarize(latestUpdate = max(entry_time)) %>%
            mutate(tLastUpdate=as.numeric((Sys.time()-latestUpdate)/24))%>%
            left_join(admin_lookup %>% rename(country=admin0) %>% select(country),.) %>%
            distinct %>%
            mutate(tLastUpdate=ifelse(is.na(tLastUpdate),999,tLastUpdate)) %>%
            mutate(tLastUpdate=cut(tLastUpdate,c(0,1,7,14,999,1000),right = FALSE,
                                   labels=c("<24 hrs","1-7 days",
                                            "8-14 days",">2 weeks","Never")))
        
        ## bring in world map
        ## from https://exploratory.io/map
        world <- geojsonio::geojson_read("world.geojson", what = "sp") %>% st_as_sf 
        world <- merge(world,simp_dat2 %>% rename(ISO_A3=country)) 
                
        
        pal <- colorFactor(
            palette = "YlOrRd",
            domain = simp_dat2$tLastUpdate)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>time since last update: %s",
            world$NAME,world$tLastUpdate
        ) %>% lapply(htmltools::HTML)
        
        
        leaflet(world) %>% 
            addPolygons(fillColor = ~pal(tLastUpdate),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.6,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
                        ) %>%
            addLegend("bottomleft", pal = pal, values = ~tLastUpdate,
                      title = "Time since last update",
                      opacity = .6
            )
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
