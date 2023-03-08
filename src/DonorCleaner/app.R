#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(DT)
library(reticulate)
library(XML)

### options

options(shiny.maxRequestSize = 30 * 1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),

  # App title ----
  titlePanel("Donor Formatter"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(



      # Input: Select a file ----
      fileInput("donorfile", "Select a .csv, .txt, .xml, or .html Donor File",
        multiple = TRUE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          ".xml",
          ".html"
        )
      ),
      htmlOutput("donorsize"),
      htmlOutput("usabledonors"),
      htmlOutput("avg_donors"),
      htmlOutput("locations"),
      htmlOutput("missing_zips"),
      # Horizontal line ----
      tags$hr(),
      selectInput(
        "state",
        "Source:",
        list(
          `National` = list("FEC" = "FEC"),
          `City` = list(
            "Atlanta" = "ATL",
            "Los Angeles" = "LA_C",
            "New York City" = "NYC",
            "Philadelphia" = "PHIL"
          ),
          `State` = list(
            "Alabama" = "AL",
            "Alaska" = "AK",
            "Arizona" = "AZ",
            "California" = "CA",
            "Colorado" = "CO",
            "Connecticut" = "CT",
            "Delaware" = "DE",
            "Florida" = "FL",
            "Georgia (Transactions)" = "GA",
            "Georgia (Finance Report)" = "GA_old",
            "Hawaii" = "HI",
            "Idaho" = "ID",
            "Illinois" = "IL",
            "Indiana" = "IN",
            "Iowa" = "IA",
            "Kansas" = "KS",
            "Kentucky" = "KY",
            "Louisiana" = "LA",
            "Maine" = "ME",
            "Massachusetts" = "MA",
            "Maryland" = "MD",
            "Michigan" = "MI",
            "Missouri" = "MO",
            "Montana" = "MT",
            "Nebraska" = "NE",
            "New Jersey" = "NJ",
            "New Mexico" = "NM",
            "New York" = "NY",
            "North Carolina" = "NC",
            "North Dakota" = "ND",
            "Ohio" = "OH",
            "Oklahoma" = "OK",
            "Oregon" = "OR",
            "Rhode Island" = "RI",
            "South Carolina" = "SC",
            "Tennessee" = "TN",
            "Utah" = "UT",
            "Virginia" = "VA",
            "Vermont" = "VT",
            "Washington" = "WA",
            "West Virginia" = "WV",
            "Wisconsin" = "WI",
            "Wyoming" = "WY"
          )
        ),
        selected = "Alabama",
      ),
      
      conditionalPanel(
        condition = "input.state == 'PHIL'",
        textInput("philly_can", "Philadelphia Candidate")
        
        ),

      # Button
      downloadButton("downloadData", "Download"),
      tags$a(href = "https://jer164.github.io/donoRs/", "User Guide", target = "_blank")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      dataTableOutput("contents")
    )
  )
)


server <- function(input, output) {
  source("transforms.R")
  
  
  candidate <- reactive({
    input$philly_can
  })
  
  state_fin <- reactive({
    input$state
  })

  #### Create DataTable on Output

  output$contents <- renderDataTable({
    
    
    df <- datasetInput() %>%
      select_if(function(x) !(all(is.na(x)) | all(x == "")))
      
    
  })

  ##### Create reactive dataset for download

  
  datasetInput <- reactive({
    
    
    if (input$state == 'PHIL'){
      donors_df <- donor_cleaner(input$philly_can, state_fin())}
    else{
      donors_df <- donor_cleaner(input$donorfile$datapath, state_fin())}
    
    
  })
  
  

  ###### Create download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("candidate_name_formatted.csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE, na = "")
    }
  )

  output$donorsize <- renderText({
    paste("<b>Number of Donors: </b>", nrow(datasetInput()))
  })
  
  output$usabledonors <- renderText({
    paste("<b>Number of ABBA-Friendly Donors: </b>", sum(complete.cases(subset(datasetInput(), select = c("full_name", "first_name", "last_name")))))
  })
  
  output$avg_donors <- renderText({
    paste("<b>Average Donation Amount: </b>", round(mean(datasetInput()$donation_amount), 3))
  })
  
  output$locations <- renderText({
    paste("<b>Most Frequent State: </b>", datasetInput() %>% count(state) %>% slice_max(n = 1, order_by = n) %>% pull(state))
  })
  
  output$missing_zips <- renderText({
    paste("<b>Missing Zips: </b>", datasetInput() %>% pull(zip) %>% anyNA())
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)
