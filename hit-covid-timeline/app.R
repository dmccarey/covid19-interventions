## global variables
app_title   <- "HIT-COVID"

## load all .R files in source directory
source("source/utils.R")
reload_source()

## some look table for countries and admin units
admin_lookup <- read_csv("geo_lookup.csv")
country_names <- setNames(admin_lookup$admin0,nm =admin_lookup$NAME_0 ) #%>% na.omit


## load required packages not in reload_source()
if(!require(pacman)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
p_load(shiny,readr,purrr,DT,markdown)

## set up data
long_data <- get_long_data(fresh_pull = FALSE,long_file_path = "generated_data/survey_data_long.csv")
last_updated_time <- file.info("generated_data/survey_data_long.csv")$mtime

interven_df_table <- long_data   %>% 
    filter(complete == "Complete") %>% 
    select(record_id, entry_time = geography_and_intro_timestamp,
           national_entry, country, admin1 = adm1,
           locality = adm_lowest, intervention, intervention_specific,
           status, subpopulation = pop, date_of_update = t,
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
            selectInput("admin_unit",label = "Select an admin1 unit:",choices = NULL,selectize = TRUE),
             checkboxInput("include_national", label ="Include National Interventions?", value = TRUE, width = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            includeMarkdown("include/heading_box.md"),
            br(),
            plotOutput("timeline"),
            p("Below is a table of all the data to explore:"),
            dataTableOutput("overview_tab")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {


    country_select <- reactive({
        filter(admin_lookup, admin0 == input$country_select)
    })
    
    observeEvent(country_select(), {
        tmp<-country_select()
        choices <- setNames(tmp[,"GID_1"],nm=tmp[,"NAME_1"] ) %>% na.omit
        updateSelectInput(session,"admin_unit", choices = choices) 
    })
    
    output$overview_tab <- renderDT(
        interven_df_table,
        class = "display nowrap compact", # style
        filter = "top", # location of column filters,
        options = list(  # options
            scrollX = TRUE # allow user to scroll wide tables horizontally
        )
        
    )
    
    ## make timeline plot
    output$timeline <- renderPlot({
        
        tmp <- interven_df_table
        
        if(input$include_national){
            # figure out which country
            cntry <- admin_lookup %>% dplyr::filter(GID_1==input$admin_unit) %>% select(admin0) %>% unlist %>% first
            #cntry <- interven_df_table %>% filter(admin1==admin_unit) %>% select(country) %>% unlist %>% first
            
            national_ints <- tmp %>% dplyr::filter(national_entry=="Yes" & country==cntry) %>% 
                mutate(admin1 = input$admin_unit)
            
            #if this is zero length (e.g., nothing has been entered yet), we make a 
            
            
            tmp<-bind_rows_keep_factors(tmp,national_ints)
            # get national observations
        }
        
        
        # can add back ggplotly later but this isn't so pretty without munging
        #ggplotly(
            ggplot(data = tmp %>% filter(admin1 %in% input$admin_unit),
               aes(x = date_of_update, y = intervention_specific,
                   shape = status_simp, 
                   color = national_entry)) +
            geom_point(size = 3) + 
            xlab("date of policy change") + 
            ylab("") + theme(legend.position="bottom") + theme_bw() + labs(color="National Policy?",shape="Policy Status")
        #)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
