# Basics ####

rm(list = ls())

library(tidyverse)
library(shiny)

# UI ####

ui <- navbarPage('BGSE students',id = "inTabset",
                 tabPanel(title = "Table", value = "panel1", 
                           dataTableOutput('supervisor_table')
                 )
)

# Server ####

server <- function(input, output, session) {
  
  # (commented out) list of reactive values ####
  
  #list_of_reactive_values <- reactiveValues() #this is a list
  
  # Run script to get supervisor_table
  
  source("../supervisors.R")
  
  # Table ####
  
  output$supervisor_table <- renderDataTable(supervisor_table)
  
}


# Shiny app ####

shinyApp(ui = ui, server = server)