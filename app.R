#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
library(lubridate)
library(dplyr)
library(shiny)
library(data.table)
library(DT)
library(readxl)
source("DroneTaggingFilterFunctions.r")
isLocal <- TRUE
# dataPath <- setDataPath(isLocal = isLocal, localPath = "DroneDataFilter/data/", shinyPath = "data\\")
dataPath <- setDataPath(isLocal = isLocal, localPath = "data/", shinyPath = "data\\")

txtignoredTab <- read_file(paste0(dataPath, "IgnoredTab.txt"))
txtCleanTab <- read_file(paste0(dataPath, "cleanData.txt"))
txtTagKeyTab <- read_file(paste0(dataPath, "keyForDroneTagTab.txt"))
computeFlag = FALSE # if TRUE load raw data and then save RDS file. if FALSE, load RDS files.
if (computeFlag) { # load raw data
  data1 <- data.table::data.table(readRDS(paste0(dataPath, "data.rds"))$data) %>% select(-c("Collection"))
  keyfordronetags <- read_excel(paste0(dataPath, "keyfordronetags.xlsx"), range = "A1:C3000")[, 1:3]
  keyfordronetags <- keyfordronetags[complete.cases(keyfordronetags),] %>% 
    mutate(hive = factor(hive), OriginalUID = UID) %>% 
    mutate(OriginalUIDLength = nchar(OriginalUID))
} else {  # load RDS files
      cleanData <- readRDS(paste0(dataPath, "cleanData.rds"))
      ignoredData <- readRDS(paste0(dataPath, "ignoredData.rds"))
      keyfordronetags <- readRDS(paste0(dataPath, "keyfordronetags.rds"))
      summaryByBee <- readRDS(paste0(dataPath, "SummaryByBee.rds"))
  }

if (computeFlag) {
  dataList <- massageData2(data1, keyfordronetags)
  ignoredData <- dataList$ignoredData
  cleanData <- dataList$cleanData
  keyfordronetags <- dataList$keyfordronetags
  summaryByBee <- dataList$summaryByBee
  rm(dataList)
  saveRDS(ignoredData, paste0(dataPath, "ignoredData.rds"))
  saveRDS(cleanData, paste0(dataPath, "cleanData.rds"))
  saveRDS(keyfordronetags, paste0(dataPath, "KeyForDroneTags.rds"))
  saveRDS(summaryByBee, paste0(dataPath, "SummaryByBee.rds"))
}

# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("Drone Data Filter"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            actionButton("exitBtn2", "Exit", class = "btn-danger")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Clean",
                               downloadButton("downloadCleanData", "Download"),
                               hr(),
                               DTOutput("tblCleanData"),
                               hr(),
                               verbatimTextOutput("txtCleanTab")),
                      tabPanel("Ignored", 
                               downloadButton("downloadIgnoredData", "Download"),
                               hr(),
                               DTOutput("tblIgnoredData"),
                               hr(),
                               verbatimTextOutput("txtignoredTab")),
                      tabPanel("KeyTagRaw",
                               downloadButton("downloadKeyTagRawData", "Download"),
                               hr(),
                               DTOutput("tblKeyForDroneTags"),
                               hr(),
                               verbatimTextOutput("txtTagKeyTab")),
                      tabPanel("SummaryByBee",
                               downloadButton("downloadSummaryByBeeData", "Download"),
                               hr(),
                               DTOutput("tblSummaryByBee"),
                               hr(),
                               h6("Summary of: Summary By Bee"),
                               verbatimTextOutput("sumSummaryByBee")),
                      tabPanel("Helpful Info",
                               tags$iframe(
                                 seamless = "seamless",
                                 src = "https://docs.google.com/document/d/1sK2BIwgYC30bHMLp5fDCAmmA0ZvnG4EugI2CyqDriYo/edit?usp=sharing",
                                 height = 800, width = 1400
                               )),
                      tabPanel("Study Questions",
                               tags$iframe(
                                 seamless = "seamless",
                                 src = "https://docs.google.com/document/d/1nZ-yIYl4ifCDeKuu2IeLEtsU72wHCPvqAvjaWZowxwg/edit?usp=sharing",
                                 height = 800, width = 1400))
      )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    output$tblCleanData <- DT::renderDT(cleanData, 
                                    filter = "top", options = list(pageLength = 10))
    observeEvent(input$exitBtn2, {stopApp(0)})
    output$tblIgnoredData <- DT::renderDT(ignoredData,  
                                     filter = "top", options = list(pageLength = 10))
    output$txtIgnoredTab <- renderText(txtIgnoredTab)
    output$txtCleanTab <- renderText(txtCleanTab)
    output$txtTagkeyTab <- renderText(txtTagKeyTab)
    output$tblKeyForDroneTags <- DT::renderDT(data.table(keyfordronetags),
                                              filter = "top", options = list(pageLength = 10))
    output$tblSummaryByBee <- DT::renderDT(summaryByBee,
                                           filter = "top", options = list(pageLength = 10))
    output$sumSummaryByBee <- renderPrint({
      summary(summaryByBee %>% 
                mutate(Hive = factor(Hive), FirstReader = factor(FirstReader), FirstDirection = factor(FirstDirection),
                       FirstAnt1 = factor(FirstAnt1), FirstAnt2 = factor(FirstAnt2)) %>% 
                select(nObs, AgeFirst, AgeLast, Hive, FirstReader, FirstDirection, FirstAnt1, FirstAnt2))  
    })
    output$downloadCleanData <- downloadHandler(filename = paste0("CleanData_", Sys.Date(), ".csv"),  
                                                content = function(file) {write.csv(cleanData, file, row.names = FALSE)})
    output$downloadIgnoredData <- downloadHandler(filename = paste0("IgnoredData_", Sys.Date(), ".csv"),  
                                                content = function(file) {write.csv(ignoredData, file, row.names = FALSE)})
    output$downloadKeyTagRawData <- downloadHandler(filename = paste0("TagKeyData_", Sys.Date(), ".csv"),  
                                                content = function(file) {write.csv(keyfordronetags, file, row.names = FALSE)})
    output$downloadSummaryByBeeData <- downloadHandler(filename = paste0("SummaryByBeeData_", Sys.Date(), ".csv"),  
                                                content = function(file) {write.csv(summaryByBee, file, row.names = FALSE)})
    
    # web pages
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

