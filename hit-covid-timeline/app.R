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
long_data <- get_long_data(fresh_pull = FALSE,long_file_path = "generated_data/survey_data_long.csv")
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
    titlePanel(sprintf("HIT-COVID Data Viewer (last updated %s)",last_updated_time)),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Choose a location:", style = sprintf("color:%s", "steelblue")),
            selectInput("country_select",label = "Select a country:",choices = country_names,selectize = TRUE),
            selectInput("admin_unit",label = "Select an admin1 unit:",choices = admin_names,selectize = TRUE),
             checkboxInput("include_national", label ="Include National Interventions?", value = TRUE, width = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Overview",
                         br(),
                         h2("Welcome to the Health Intervention Tracking for COVID-19 Application"),
                         h3(sprintf("%.0f interventions logged",nrow(interven_df_plot))),
                         h3(sprintf("%.0f countries covered",n_distinct(interven_df_plot$country))),
                         br(),
                         downloadButton("download_data", label = "Download Current Data")
                    ),
                tabPanel("Timeline",
            includeMarkdown("include/heading_box.md"),
            br(),
            girafeOutput("timeline"),
            p("Below is a table of all the data to explore:"),
            dataTableOutput("overview_tab")
                ),
            tabPanel("Map",
                     leafletOutput("simp_map")
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
    
    ## map
    
    output$simp_map <- renderLeaflet({
        ## just a quick one with where we have data right now
        ## and last update
        simp_dat <- long_data %>% 
            group_by(country) %>% 
            summarize(n = n(),
                      n_national = sum(national_entry=="Yes"),
                      n_admin1 = n_distinct(adm1)) %>% 
            left_join(admin_lookup %>% rename(country=admin0) %>% select(country),.) %>% 
            distinct %>% 
            mutate(n=replace_na(n,0),
                   n_national=replace_na(n_national,0),
                   n_admin1 = replace_na(n_admin1,0)
            )
        ## bring in world map
        ## from https://exploratory.io/map
        world <- geojsonio::geojson_read("world.geojson", what = "sp") %>% st_as_sf 
        
        wd = left_join(world,simp_dat %>% rename(ISO_A3=country))
        
        pal <- colorNumeric(
            palette = "YlOrRd",
            domain = simp_dat$n)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%s intervention changes logged<br/>%s intervention changes logged nationally <br/>%s administrative units with data",
            wd$NAME,wd$n,wd$n_national, wd$n_admin1
        ) %>% lapply(htmltools::HTML)
        
        
        leaflet(wd) %>% 
            addPolygons(fillColor = ~pal(n),
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
                            direction = "auto")) %>% 
            addLegend("bottomleft", pal = pal, values = ~n_national,
                      title = "Number of interventions logged",
                      opacity = .6
            )
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
