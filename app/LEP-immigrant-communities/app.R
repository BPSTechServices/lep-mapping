library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafsync)

## Sync in shiny: https://stackoverflow.com/questions/40272513/display-of-sync-mapview-in-shiny 

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

lep_and_birthplace_method_overlays <- readRDS("data/lep_and_birthplace_method_overlays_2019.rds")
sf::st_crs(lep_and_birthplace_method_overlays) <- 4326

lep_and_birthplaces <- readRDS("data/lep_and_birthplaces_2019.rds") %>% select(-NAME) 
sf::st_crs(lep_and_birthplaces) <- 4326

choices_lang_bpl <- lep_and_birthplaces %>% 
    filter(!(variable %in% c("ttl_speakers_base", "total"))) %>% 
    pull(variable) %>% unique()

# Define UI for application that maps LEP languages and birthplaces of select groups
ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "LEP & Immigrant Community Explorer", titleWidth = "380"),
    dashboardSidebar(
        div(style="padding: 15px 15px 15px 15px;", "This app allows you to map concentrations of LEP speakers by target languages to census tracts. Data does not exist for all language categories of which Portland has a significant population. To help infer critical languages, this app also allows you to map concentrations of the foreign-born population for target immigrant communities. The bottom-right map is spatial-statistical overlay of where we would suggest translation and interpretation services at a localized level. This app is intended to be a supplement to official ", tags$a(href="https://www.portlandoregon.gov/oehr/80870","OEHR language guidance"), "."),
        selectInput(inputId = 'user_lang_bpl', 
                                 label = 'Select LEP Language or Immigrant Birthplace', 
                                 selected = 'Vietnamese', 
                                 choices = choices_lang_bpl),
        div(style="padding: 15px 15px 15px 15px;", "Note: Percentage estimates for country of birthplace are expressed as a share of total population in that census tract, not of foreign-born population. Density is expressed as people per square mile."),
        div(style="padding: 15px 15px 15px 15px;", "Source: 2015-19 ACS 5-year estimates, Tables B05006 and C16001. Prepared March 24, 2021 by Portland Bureau of Planning & Sustainability.")),
    dashboardBody(uiOutput("syncmap"))
)



# Define server logic required to map
server <- function(input, output) {
    
    filtered_overlay <- reactive({
        lep_and_birthplace_method_overlays %>%
            filter(language_or_birthplace == input$user_lang_bpl)
    })
    
    
    filtered_data <- reactive({
        lep_and_birthplaces %>%
            filter(variable == input$user_lang_bpl)
    })
    
    
    output$syncmap <- renderUI({
        m1 <- mapview(filtered_data(), zcol = "estimate", layer.name = paste0(input$user_lang_bpl,"-#")) %>%
            .@map %>% leaflet::setView(-122.62, 45.54, zoom = 10) 
        m2 <- mapview(filtered_data(), zcol = "share", layer.name = paste0(input$user_lang_bpl,"-%")) %>%
            .@map %>% leaflet::setView(-122.62, 45.54, zoom = 10) 
        m3 <- mapview(filtered_data(), zcol = "density", layer.name = paste0(input$user_lang_bpl,"-density")) %>%
            .@map %>% leaflet::setView(-122.62, 45.54, zoom = 10) 
        m4 <- mapview(filtered_overlay(), layer.name = paste0(input$user_lang_bpl," overlay")) %>%
            .@map %>% leaflet::setView(-122.62, 45.54, zoom = 10) 
        
        sync(m1, m2, m3, m4)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
