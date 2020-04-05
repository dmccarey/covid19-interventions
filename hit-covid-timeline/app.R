## load functions and source packages
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
    left_join(interven_names, by = "intervention_specific") %>%
    select(record_id, entry_time = geography_and_intro_timestamp,
           national_entry, country, country_name, admin1 = adm1, admin1_name,
           locality = adm_lowest, intervention_clean,
           date_of_update = t, status, subpopulation = pop,
           required, enforcement, details) %>%
    mutate(status_simp = ifelse(status %in% c("closed", "fully closed",
                                              "fully restricted", "all",
                                              "required", "yes"), 3,
                                ifelse(status %in% c("partially closed", "partially restricted",
                                                     "recommended", "some"), 2,
                                       ifelse(status %in% c("open", "no", "no policy"), 1, status))),
           status_simp = factor(status_simp, levels = c(1, 2, 3),
                                labels = c("open/no/no policy",
                                           "partially closed/partially restricted/\nrecommended/some",
                                           "closed/restricted/all/yes")))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(sprintf("Health Intervention Tracking for COVID-19 Data Viewer (last updated %s)",last_updated_time)),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3, 
            div(style = "font-size: 8px;", 
                sliderInput(inputId = "groups", 
                            label = "No. of Groups",
                            value = 4, min = 2, max = 12)
            ),  
            tags$style(tableHTML::make_css(list('.well', 'border-width', '0px'))),
            h2("Choose a location:", style = sprintf("color:%s", "steelblue")),
            selectInput("country_select",label = "Select a country:",choices = country_names,selectize = TRUE),
            selectInput("admin_unit",label = "Select an admin1 unit:",choices = admin_names,selectize = TRUE),
             checkboxInput("include_national", label ="Include National Interventions?", value = TRUE, width = NULL),
            br(),
            downloadButton("download_data", label = "Download Current Data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Overview",
                         br(),
                         #h3("Background"),
                         #h4("As the COVID-19 pandemic unfolds, massive government efforts are being made globally to try to reduce morbidity and mortality. Governments have taken a large range of actions, from broad-scale social distancing such as the forced lockdown of cities with mandatory home confinement, to behavior change campaigns to improve hand hygiene. Moreover, governments have implemented these measures at different points in time during the course of their epidemic."),
                         #h4("Major government mandated actions come with huge economic risks and many are asking if some of the most drastic actions are worth it. In order to start to understand how different public health policy interventions may have influenced COVID-19 transmission across the globe, we need detailed data on when and where specific policy interventions have been enacted over the course of this epidemic. The goal of this project is to provide a comprehensive database of public health policy at the first level administrative unit, to serve as a key component of assessments of the impact of these policies on COVID transmission dynamics and other changes in the health of affected populations. This living database will be maintained throughout the course of the pandemic with visual summaries of raw data made available publicly. "),
                         #h3("Progress"),
                         h4(sprintf("%.0f interventions logged",nrow(interven_df_plot))),
                         h4(sprintf("%.0f countries covered",n_distinct(interven_df_plot$country))),
                         br(),
                         leafletOutput("simp_map"),
                         h4("Figure. Overview of recent updates")
                         #plotOutput('recordHeatmap')
                    ),
                tabPanel("Timeline",
            includeMarkdown("include/heading_box.md"),
            br(),
            girafeOutput("timeline"),
            p("Below is a table of all the data to explore:"),
            dataTableOutput("overview_tab")
                ),
            tabPanel("Maps",
                     p("More to come soon")
                     )
            ))))

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
        tmp<-country_select()
        choices <- setNames(tmp$GID_1,nm=tmp$NAME_1 ) #%>% na.omit
        updateSelectInput(session,"admin_unit", choices = choices) 
    })
    
    output$overview_tab <- renderDT(
        interven_df_plot   %>% 
            filter(country == input$country_select) %>%
            select(-national_entry, -status_simp, -country, -admin1) %>%
            arrange(admin1_name),
        class = "display nowrap compact", # style
        filter = "top", # location of column filters,
        options = list(  # options
            scrollX = TRUE # allow user to scroll wide tables horizontally
        )
        
    )
    
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
        tmp <- tmp %>% mutate(tooltip=paste0("record id: ",record_id,"\n",
                                             "date: ",date_of_update,"\n",
                                             "intervention: ",intervention_clean,"\n",
                                             "status: ",status,"\n",
                                             "subpopulation: ",subpopulation,"\n",
                                             "required:",required,"\n",
                                             "enforcement:",enforcement))
            
        gg <- ggplot(data = tmp %>% filter(admin1 %in% input$admin_unit),
               aes(x = date_of_update, y = intervention_clean,
                   shape = status_simp, 
                   color = national_entry)) +
                geom_point_interactive(aes(tooltip = tooltip,
                                           data_id = record_id),size=3)+
            #geom_point(size = 3) + 
            xlab("Date of policy change") + 
            ylab("") + theme(legend.position="bottom") + theme_bw() + labs(color="National Policy?",shape="Policy Status")
        
            girafe(ggobj = gg,width_svg = 12)

    })
    
    ## making simple map
    
    output$simp_map <- renderLeaflet({
        ## just a quick one with where we have data right now
        ## and last update
        # simp_dat <- long_data %>%
        #     group_by(country) %>%
        #     summarize(n = n(),
        #               n_national = sum(national_entry=="Yes"),
        #               n_admin1 = n_distinct(adm1)) %>%
        #     left_join(admin_lookup %>% rename(country=admin0) %>% select(country),.) %>%
        #     distinct %>%
        #     mutate(n=replace_na(n,0),
        #            n_national=replace_na(n_national,0),
        #            n_admin1 = replace_na(n_admin1,0)
        #     )
        # 
        # ## bring in world map
        # ## from https://exploratory.io/map
        # world <- geojsonio::geojson_read("world.geojson", what = "sp") %>% st_as_sf 
        # 
        # wd = left_join(world,simp_dat %>% rename(ISO_A3=country))
        # 
        # pal <- colorNumeric(
        #     palette = "YlOrRd",
        #     domain = simp_dat$n)
        # 
        # labels <- sprintf(
        #     "<strong>%s</strong><br/>%s intervention changes logged<br/>%s intervention changes logged nationally <br/>%s administrative units with data",
        #     wd$NAME,wd$n,wd$n_national, wd$n_admin1
        # ) %>% lapply(htmltools::HTML)
        # 
        # 
        # leaflet(wd) %>% 
        #     addPolygons(fillColor = ~pal(n),
        #                 weight = 2,
        #                 opacity = 1,
        #                 color = "white",
        #                 dashArray = "3",
        #                 fillOpacity = 0.6,
        #                 highlight = highlightOptions(
        #                     weight = 5,
        #                     color = "#666",
        #                     dashArray = "",
        #                     fillOpacity = 0.7,
        #                     bringToFront = TRUE),
        #                 label = labels,
        #                 labelOptions = labelOptions(
        #                     style = list("font-weight" = "normal", padding = "3px 8px"),
        #                     textsize = "15px",
        #                     direction = "auto")) %>% 
        #     addLegend("bottomleft", pal = pal, values = ~n_national,
        #               title = "Number of interventions logged",
        #               opacity = .6
        #     )
        # 
        
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
