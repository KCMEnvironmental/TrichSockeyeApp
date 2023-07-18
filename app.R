# TrichAnalytics Shiny App: Otolith Data - Sockeye
# Created 18 July 2023 KMill 
#
# Generates and saves two plots per sample ID: (1) Zn and (2) Ba and Sr intensity vs time.
#
# To run App: 
#   Click the 'Run App' button above
#   Use 'Browse' button to upload data file (report style file)
#   Use 'Explore Data' section to see graphs for sample IDs as selected from dropdown menu 
#   Hit 'Download All Plots' button to generate and save plots
#
#UPDATES: Potentially modify to allow 'default' or 'auto-select' of actual data range for x axis (length). Might add value if user does not want to select range each time. Maybe add selection box; if user selects 'auto' then no numeric input is shown; if user selects 'manually set range' then numeric input is shown. 

# Load Libraries ----
library(shiny)
library(shinythemes) 
library(shinyWidgets)
library(shinyFiles)
library(ggplot2)
library(tidyverse)
library(zip)
library(ggiraph)
library(viridis)
library(openxlsx)


# Define UI for application ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("TrichAnalytics Otolith Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", 
                label = "Upload Data File"), 
      numericInput(inputId = "xmin", 
                   label = "X-axis Minimum (µm)",
                   value = 0), # Set to automatically select minimum of dataframe
      numericInput(inputId = "xmax", 
                   label = "X-axis Maximum (µm)", 
                   value = 0), # Set to automatically select maximum of dataframe
      numericInput(inputId = "Znmax", 
                   label = "Zinc Y-axis Maximum (ppm)", 
                   value = 250), 
      numericInput(inputId = "Srmax", 
                   label = "Strontium Y-axis Maximum (ppm)", 
                   value = 3000), 
      numericInput(inputId = "Bamax", 
                   label = "Barium Y-axis Maximum (ppm)", 
                   value = 50), 
      downloadButton(outputId = "download_btn", 
                     label = "Download All Plots", 
                     icon = icon("fish-fins"))
      ), 
    mainPanel(textOutput('text'),
              tags$head(tags$style("#text{font-style: italic;}")),
              uiOutput(outputId = "dropdown"), 
              fluidRow(
                column(6, girafeOutput(outputId = "plot_Zn")), 
                column(6, girafeOutput(outputId = "plot_SrBa")))
    )
  )
)
  
# Define server logic ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  output$text <- renderText({
    req(is.null(input$file))
    "Upload file to browse plots"
  })
  
## Download button ----
  
output$download_btn <- downloadHandler(
  filename = function() {
    paste(Sys.Date(), ".zip", sep = "")
  },
  content = function(file) {
    sampleID_list <- readxl::excel_sheets(input$file$datapath)[readxl::excel_sheets(input$file$datapath) %in% 
                                                                 c("Otolith Analysis",
                                                                   "Sample Set Information", 
                                                                   "COC", 
                                                                   "Sample Notes") == FALSE]
    temp_directory <- file.path(tempdir(), as.integer(Sys.time())) 
    dir.create(temp_directory) # Creates temporary directory to save plots; allows for downloading a zip file of all plots
    
    for (i in sampleID_list) {   
      
      data <- readxl::read_excel(input$file$datapath, sheet = i)[-c(1:2),] %>% 
        rename("length" = "Parameter") 
      
      ### Create and save zinc plot ----
      plot_Zn <- ggplot() + 
        geom_line(data = data, 
                   aes(x = as.numeric(length), 
                       y = as.numeric(`66Zn`)), 
                   colour = "darkgreen") + 
        theme_bw() +
        labs(y = "Zinc (ppm)", 
             x = "Length (µm)", 
             title = i) +
        theme(aspect.ratio = 1, 
              plot.title = element_text(face = "bold", 
                                        size = 14)) +
        scale_y_continuous(limits = c(0, input$Znmax))
      
      ggsave(paste(temp_directory, "/", i, "_Zn.png", sep = ""), plot_Zn)
      
      ### Create and save strontium and barium plot ----
      yaxisCxn <- input$Srmax/input$Bamax
      plot_SrBa <- ggplot() + 
        geom_line(data = data, 
                   aes(x = as.numeric(length), 
                       y = as.numeric(`88Sr`), 
                       colour = "Strontium")) + 
        geom_line(data = data, 
                   aes(x = as.numeric(length), 
                       y = as.numeric(`137Ba`)*yaxisCxn, 
                       colour = "Barium")) + 
        theme_bw() +
        labs(x = "Length (µm)", 
             title = i) +
        theme(aspect.ratio = 1, 
              plot.title = element_text(face = "bold", 
                                        size = 14, 
                                        margin = margin(0,0,-13,0)), 
              legend.title = element_blank(), 
              legend.position = "top", 
              legend.margin = margin(0,0,0,150),
              legend.box.spacing = unit(0, 'cm')) +
        scale_y_continuous(name = "Strontium (ppm)",
                           limits = c(0, input$Srmax), 
                           sec.axis = sec_axis(~ . /yaxisCxn, name = "Barium (ppm)"))
      
      ggsave(paste(temp_directory, "/", i, "_SrBa.png", sep = ""), plot_SrBa)
    }
    
    zip::zip(
      zipfile = file,
      files = dir(temp_directory),
      root = temp_directory
    )}, 
  contentType = 'application/zip')
 

# Dropdown button ----
  output$dropdown <- renderUI({
    req(input$file)
    selectInput('sampleIDs', 
                "Sample ID", 
                readxl::excel_sheets(input$file$datapath)[readxl::excel_sheets(input$file$datapath) %in% 
                                                            c("Otolith Analysis",
                                                            "Sample Set Information", 
                                                            "COC", 
                                                            "Sample Notes") == FALSE]) 
  })
  
  ### Create zinc plot ----
  output$plot_Zn <- renderGirafe({
    req(input$file)
    data <- readxl::read_excel(input$file$datapath,
                               sheet = input$sampleIDs)[-c(1:2),] %>% 
      rename("length" = "Parameter") %>% 
      mutate(length = as.numeric(length))
    
  data$tp <- (paste0(round(data$length, 0), " s \n", round(data$`66Zn`, 0), " ppm"))
    
   plot_Zn <- ggplot(data = data, 
               aes(x = as.numeric(length), 
                   y = as.numeric(`66Zn`))) + 
    geom_line(colour = "darkgreen") + 
    geom_point_interactive(aes(tooltip = tp), 
                           alpha = 0) +
    theme_bw() +
    labs(y = "Zinc (ppm)", 
         x = "Length (µm)", 
         title = input$sampleIDs) +
    theme(aspect.ratio = 1, 
          plot.title = element_text(face = "bold", 
                                    size = 14)) +
    scale_y_continuous(limits = c(0, input$Znmax))
   
   ggiraph(code = print(plot_Zn))
   
  })
  
  
  ### Create Sr Ba plot ----
  output$plot_SrBa <- renderGirafe({
    
    req(input$file)
    
    yaxisCxn <- input$Srmax/input$Bamax
    
    data <- readxl::read_excel(input$file$datapath,
                               sheet = input$sampleIDs)[-c(1:2),] %>% 
      rename("length" = "Parameter") %>% 
      mutate(length = as.numeric(length))
    
    data$tp <- (paste0(round(data$length, 0), " s \n", round(data$`88Sr`, 0), " ppm Sr \n", round(data$`137Ba`, 1), " ppm Ba"))
    
    plot_SrBa <- ggplot() + 
      geom_line(data = data, 
                 aes(x = as.numeric(length), 
                     y = as.numeric(`88Sr`), 
                     colour = "Strontium")) + 
      geom_point_interactive(data = data, 
                             alpha = 0, # set totally transparent so viewer sees only geom_line with tooltip and does not see geom_point. 
                             aes(x = as.numeric(length), 
                                 y = as.numeric(`88Sr`), 
                                 tooltip = tp)) + 
      geom_line(data = data, 
                 aes(x = as.numeric(length), 
                     y = as.numeric(`137Ba`)*yaxisCxn, 
                     colour = "Barium")) + 
      geom_point_interactive(data = data, # duplicates tooltip for Ba series, so tooltip will show when hovering over any data point
                             alpha = 0, 
                             aes(x = as.numeric(length), 
                                 y = as.numeric(`137Ba`)*yaxisCxn, 
                                 tooltip = tp)) + 
      theme_bw() +
      labs(x = "Length (µm)", 
           title = input$sampleIDs) +
      theme(aspect.ratio = 1, 
            plot.title = element_text(face = "bold", 
                                      size = 14, 
                                      margin = margin(0,0,-13,0)), 
            legend.title = element_blank(), 
            legend.position = "top", 
            legend.margin = margin(0,0,0,150),
            legend.box.spacing = unit(0, 'cm')) +
      scale_y_continuous(name = "Strontium (ppm)",
                         limits = c(0, input$Srmax), 
                         sec.axis = sec_axis(~ . /yaxisCxn, name = "Barium (ppm)"))
    
    ggiraph(code = print(plot_SrBa))
    
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)


