rm(list = ls())

#### > 0. PACKAGES ####
library(shiny)
library(tidyverse)
library(leaflet)
library(readODS)
library(readr)
library(readxl)
library(openssl)
library(htmltools)
library(arrow)
library(maptools)
library(shinydashboard)
library(sf)
library(plotbiomes)
library(rnaturalearth)
library(rnaturalearthdata)
library(trustedtimestamping)
library(DT)
library(dplyr)

options(shiny.maxRequestSize = 100 * 1024^2)

# TODO Not urgent: Add option to submit only vegetation data

#### > 1. FUNCTIONS ####
# Three set of functions used to control the input dataset. 
check.errors = function(DATAFRAMES, sheet, 
                        control.list, error.list){
  for(c in names(control.list)){
    type = unlist(control.list[[c]]$type)
    d = DATAFRAMES[[sheet]] |> 
      filter(!is.na(!!as.symbol(c)))
    if(nrow(DATAFRAMES[[sheet]][,c])>0){
      if(type == 'NO_CONTROL'){
        error.list$Format[[sheet]][[c]]$errors = 'all good'
      }else if(type == 'NUMERIC'){
        errors = d |>
          filter(is.na(as.numeric(!!as.symbol(c))))
        
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = 'Should be numerical!')
        }
      }else if(type == 'RANGE'){
        errors = d |>
          filter(is.na(as.numeric(!!as.symbol(c)))) |>
          add_row(
            d |> 
              filter(as.numeric(!!as.symbol(c)) < as.numeric(control.list[[c]][['control']][[1]]) |
                       as.numeric(!!as.symbol(c)) > as.numeric(control.list[[c]][['control']][[2]])
              )
          )
        
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = 'Values out of range')
        }
      }else if(type == 'YEAR'){
        errors = d |>
          filter(is.na(as.numeric(!!as.symbol(c)))) |>
          add_row(
            d |> 
              filter(as.numeric(!!as.symbol(c)) < as.numeric(control.list[[c]][['control']][[1]])
              )
          )
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = 'Values out of range')
        }
      }else if(type == 'BINOMIAL'){
        errors = d |>
          filter(!(str_to_upper(!!as.symbol(c)) %in% c('YES', 'NO', 
                                                       'TRUE', 'FALSE',
                                                       'T', 'F',
                                                       '1', '0'
          )))
        
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = 'Values should be "YES" or "NO"')
        }
      }else if(type == 'LEVELS'){
        errors = d |>
          filter(!(str_to_upper(!!as.symbol(c)) %in% 
                     control.list[[c]][['control']]))
        
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = 'Values should be in the list')
        }
      }else if(type == 'EMAIL'){
        errors = d |>
          filter(!str_detect(!!as.symbol(c), '@'))
        
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = "The email isn't valide")
        }
      }else if(type == 'ORCID'){
        errors = d |>
          filter(nchar(as.character(!!as.symbol(c))) != 19)
        
        if(nrow(errors) == 0){
          error.list$Format[[sheet]][[c]]$errors = 'all good'
        }else{
          error.list$Format[[sheet]][[c]]$errors = 
            errors[,c(names(control.list)[1], c)] |> 
            mutate(error = 'This ORCID structure should be XXXX-XXXX-XXXX-XXXX')
        }
        
      }else{
        error.list$Format[[sheet]][[c]]$errors = "Empty"
      }
    }
  }
  return(error.list)
}

check.all.errors = function(DATAFRAMES, L.ERRORS, vegetation.available){
  # Metadata
  L.ERRORS = 
    check.errors(DATAFRAMES = DATAFRAMES,
                 sheet = "METADATA",
                 control.list = L.CONTROLS$METADATA,
                 error.list = L.ERRORS)
  
  # People
  L.ERRORS = 
    check.errors(DATAFRAMES = DATAFRAMES,
                 sheet = "PEOPLE",
                 control.list = L.CONTROLS$PEOPLE,
                 error.list = L.ERRORS)
  
  # Vegetation
  if(vegetation.available == T){
    # Vegetation metadata
    L.ERRORS = 
      check.errors(DATAFRAMES = DATAFRAMES,
                   sheet = "VEGETATION.METADATA",
                   control.list = L.CONTROLS$VEGETATION_METADATA,
                   error.list = L.ERRORS)
    # Vegetation data
    L.ERRORS = 
      check.errors(DATAFRAMES = DATAFRAMES,
                   sheet = "VEGETATION.DATA",
                   control.list = L.CONTROLS$VEGETATION_DATA,
                   error.list = L.ERRORS)
    # Species details
    L.ERRORS = 
      check.errors(DATAFRAMES = DATAFRAMES,
                   sheet = "SPECIES.DETAILS",
                   control.list = L.CONTROLS$SPECIES_DETAILS,
                   error.list = L.ERRORS)
  }
  return(L.ERRORS)
}

extract.errors = function(LIST, level){
  LIST.EXTRACT = 
    LIST[
      map_lgl(
        .x = LIST,
        .f = ~{.x[[2]] == level}
      )] 
  
  LIST.EXTRACT = 
    LIST.EXTRACT[
      map_lgl(
        .x = LIST.EXTRACT,
        .f = ~{
          if(is_tibble(.x[[1]])){
            TRUE
          }else if(.x[[1]] == 'all good'){
            FALSE
          }else{TRUE}
        }
      )] 
  
  L.MISSING = 
    LIST.EXTRACT[
      map_lgl(
        .x = LIST.EXTRACT,
        .f = ~{
          if(is_tibble(.x[[1]])){
            F
          }else if(.x[[1]] == 'Empty'){
            T
          }else{F}
        }
      )] |> 
    names()
  
  L.ERROR.DF =  
    LIST.EXTRACT[
      map_lgl(
        .x = LIST.EXTRACT,
        .f = ~{
          if(is_tibble(.x[[1]])){
            T
          }else{F}
        }
      )]
  
  ERRORS = 
    list(
      MISSING = L.MISSING,
      ERRORS = L.ERROR.DF
    )
  
  return(ERRORS)
}

#### > 2. BUILDING LISTING ####
# List of columns
df.columns <- read_excel("columns.xlsx",
                         sheet = "Columns_definition")

# List of errors that will be filled during the control
L.ERRORS = list(
  # Issues when opening the files
  Comptabilty = list(
    Open_sheet = list(
      METADATA = list(), PEOPLE = list(), 
      VEGETATION_METADATA = list(),
      VEGETATION_DATA = list(), SPECIES_DETAILS = list()
    )),
  
  # Issues in the files structure
  Structure = list(
    NameColumn = list(
      METADATA = list(Missing = c(), Added = c()), 
      PEOPLE = list(Missing = c(), Added = c()), 
      VEGETATION_METADATA = list(Missing = c(), Added = c()),
      VEGETATION_DATA = list(Missing = c(), Added = c()), 
      SPECIES_DETAILS = list(Missing = c(), Added = c())
    )),
  
  # Issues of formating
  Format = list()
) 

# Format specifications
for(i in unique(df.columns$Sheet)){
  L.ERRORS[['Format']][[i]] = list()
  for(j in unique(df.columns$Column[df.columns$Sheet==i])){
    L.ERRORS[['Format']][[i]][[j]] = 
      list(errors = list(),
           reference = df.columns$Level[df.columns$Sheet==i &
                                          df.columns$Column==j])
  }}

L.CONTROLS = list(METADATA = list(), 
                  PEOPLE = list(),
                  VEGETATION_METADATA = list(), 
                  VEGETATION_DATA = list(),
                  SPECIES_DETAILS = list())

# Parameters to be controlled
df.parameter.meta <- read_excel("columns.xlsx",
                                sheet = "METADATA")
for(i in colnames(df.parameter.meta)){
  L.CONTROLS$METADATA[[i]][['type']] = df.parameter.meta[1,i]
  L.CONTROLS$METADATA[[i]][['control']] =
    if(L.CONTROLS$METADATA[[i]][['type']] == 'NO_CONTROL'){
      'no'
    }else if(L.CONTROLS$METADATA[[i]][['type']] == 'NUMERIC'){
      'NUMERIC'
    }else if(L.CONTROLS$METADATA[[i]][['type']] == 'RANGE'){
      c(min = df.parameter.meta[2,i], max = df.parameter.meta[3,i])
    }else if(L.CONTROLS$METADATA[[i]][['type']] == 'BINOMIAL'){
      c(T = 'YES', F = 'NO')
    }else if(L.CONTROLS$METADATA[[i]][['type']] == 'EMAIL'){
      'email'
    }else if(L.CONTROLS$METADATA[[i]][['type']] == 'ORCID'){
      'ORCID'
    }else if(L.CONTROLS$METADATA[[i]][['type']] == 'LEVELS'){
      c(unname(unlist(df.parameter.meta[!is.na(df.parameter.meta[,i]),i])))[-1]
    }else{
      'specific'
    }
}

df.parameter.people <- read_excel("columns.xlsx",
                                  sheet = "PEOPLE")
for(i in colnames(df.parameter.people)){
  L.CONTROLS$PEOPLE[[i]][['type']] = df.parameter.people[1,i]
  L.CONTROLS$PEOPLE[[i]][['control']] =
    if(L.CONTROLS$PEOPLE[[i]][['type']] == 'NO_CONTROL'){
      'no'
    }else if(L.CONTROLS$PEOPLE[[i]][['type']] == 'NUMERICAL'){
      'numerical'
    }else if(L.CONTROLS$PEOPLE[[i]][['type']] == 'RANGE'){
      c(min = df.parameter.people[2,i], max = df.parameter.people[3,i])
    }else if(L.CONTROLS$PEOPLE[[i]][['type']] == 'BINOMIAL'){
      c(T = 'YES', F = 'NO')
    }else if(L.CONTROLS$PEOPLE[[i]][['type']] == 'EMAIL'){
      'email'
    }else if(L.CONTROLS$PEOPLE[[i]][['type']] == 'ORCID'){
      'ORCID'
    }else{
      'specific'
    }
}

df.parameter.m.vege <- read_excel("columns.xlsx",
                                  sheet = "VEGETATION_METADATA")
for(i in colnames(df.parameter.m.vege)){
  L.CONTROLS$VEGETATION_METADATA[[i]][['type']] = df.parameter.m.vege[1,i]
  L.CONTROLS$VEGETATION_METADATA[[i]][['control']] =
    if(L.CONTROLS$VEGETATION_METADATA[[i]][['type']] == 'NO_CONTROL'){
      'no'
    }else if(L.CONTROLS$VEGETATION_METADATA[[i]][['type']] == 'NUMERICAL'){
      'numerical'
    }else if(L.CONTROLS$VEGETATION_METADATA[[i]][['type']] == 'RANGE'){
      c(min = df.parameter.m.vege[2,i], max = df.parameter.m.vege[3,i])
    }else if(L.CONTROLS$VEGETATION_METADATA[[i]][['type']] == 'BINOMIAL'){
      c(T = 'YES', F = 'NO')
    }else if(L.CONTROLS$VEGETATION_METADATA[[i]][['type']] == 'EMAIL'){
      'email'
    }else if(L.CONTROLS$VEGETATION_METADATA[[i]][['type']] == 'ORCID'){
      'ORCID'
    }else{
      'specific'
    }
}

df.parameter.vege.d <- read_excel("columns.xlsx",
                                  sheet = "VEGETATION_DATA")
for(i in colnames(df.parameter.vege.d)){
  L.CONTROLS$VEGETATION_DATA[[i]][['type']] = df.parameter.vege.d[1,i]
  L.CONTROLS$VEGETATION_DATA[[i]][['control']] =
    if(L.CONTROLS$VEGETATION_DATA[[i]][['type']] == 'NO_CONTROL'){
      'no'
    }else if(L.CONTROLS$VEGETATION_DATA[[i]][['type']] == 'NUMERICAL'){
      'numerical'
    }else if(L.CONTROLS$VEGETATION_DATA[[i]][['type']] == 'RANGE'){
      c(min = df.parameter.vege.d[2,i], max = df.parameter.vege.d[3,i])
    }else if(L.CONTROLS$VEGETATION_DATA[[i]][['type']] == 'BINOMIAL'){
      c(T = 'YES', F = 'NO')
    }else if(L.CONTROLS$VEGETATION_DATA[[i]][['type']] == 'EMAIL'){
      'email'
    }else if(L.CONTROLS$VEGETATION_DATA[[i]][['type']] == 'ORCID'){
      'ORCID'
    }else{
      'specific'
    }
}

df.parameter.species <- read_excel("columns.xlsx",
                                   sheet = "SPECIES_DETAILS")
for(i in colnames(df.parameter.species)){
  L.CONTROLS$SPECIES_DETAILS[[i]][['type']] = df.parameter.species[1,i]
  L.CONTROLS$SPECIES_DETAILS[[i]][['control']] =
    if(L.CONTROLS$SPECIES_DETAILS[[i]][['type']] == 'NO_CONTROL'){
      'no'
    }else if(L.CONTROLS$SPECIES_DETAILS[[i]][['type']] == 'NUMERICAL'){
      'numerical'
    }else if(L.CONTROLS$SPECIES_DETAILS[[i]][['type']] == 'RANGE'){
      c(min = df.parameter.species[2,i], max = df.parameter.species[3,i])
    }else if(L.CONTROLS$SPECIES_DETAILS[[i]][['type']] == 'BINOMIAL'){
      c(T = 'YES', F = 'NO')
    }else if(L.CONTROLS$SPECIES_DETAILS[[i]][['type']] == 'EMAIL'){
      'email'
    }else if(L.CONTROLS$SPECIES_DETAILS[[i]][['type']] == 'ORCID'){
      'ORCID'
    }else{
      'specific'
    }
}

#### > 3. UI ####
ui <- dashboardPage(
  title = "MDB data control App",
  dashboardHeader(
    title = tags$div(
      tags$span("The Microclimate DataBase (MDB) data control App", 
                style = "
                font-family: monospace;
                font-weight: bold;
                color: white; 
                font-size: 24px; 
                text-align: center; 
                width: 100%;"),
      
      tags$img(src = "MEB-logo.svg", height = "50px", 
               style = "
               float: right; 
               padding-right: 20px;
               "),
      
      style = "
      display: flex; 
      align-items: center; 
      justify-content: center; 
      position: relative;
      width: 100%;
      "
    ),
    
    titleWidth = "100%"  
  ),
  dashboardSidebar(
    tags$style(HTML("
      .selectize-input:: {
        color: white !important;
      }
      .selectize-input {
        color: #89BE48 !important;
      }
      .selectize-dropdown-content {
        color: #0F4476 !important;
      }
      .main-sidebar {
        width: 50px !important;
      }
    ")),
    sidebarMenu(
      tags$footer(
        style = "background-color: #0F4476; padding: 10px; text-align: center; position: fixed; bottom: 0; width: 100%;",
        "B) 2025 MDB. All rights reserved. | ",
        tags$a(href = "https://meb-network.com/", "Visit our website"),
        " | Contact: ",
        tags$a(href = "mailto:MEB-network@outlook.com", "MEB-network@outlook.com")
      ),
      
      
      uiOutput("Sensor_height")
    )
  ),
  
  dashboardBody(
    
    tags$style(HTML("
      .skin-blue .main-header .navbar .sidebar-toggle {
        display: none;
      }
      .skin-blue .main-header .logo {
        background-color:#0F4476;
        color:black;
        font-size: 24px;
        text-align: center;
      }
      .skin-blue .main-header .navbar {
        background-color: #0F4476;
      }
      .skin-blue .main-sidebar {
        background-color:  #89BE48;
      }

      .content-wrapper {
        margin: 0;
        padding: 20px;
        min-height: calc(100vh - 50px); /* Full height minus header */
      }

      /* Optional: Adjust the box elements in the body for more space */
      .box {
        margin: 10px;
        padding: 20px;
        font-size: 16px; /* Increase font size for better readability */
      }
      .main-header {
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 1030; /* Ensure it appears above other elements */
      }
      
      /* Prevent content from being hidden under the fixed header */
      .content-wrapper, .main-footer {
        margin-top: 50px; /* Adjust based on header height */
      }
      /* Reduce the left margin of the body */
      .content-wrapper {
        margin-left: 25px; /* Adjust this value as needed */
      }
    ")),
    h1('Welcome to the MDB Data Control App'),
    p('This app guides you through preparing your data for submission in three steps (I-III).'),  
    p('I - Upload your data in the INPUT tab: (1) select the data to be submitted and the template setting used, (2) upload your template.'),  
    p('II - Review and control your data, including geolocation and temporal coverage, in the SUMMARY tab.'),  
    p('III - Identify and correct any remaining issues in the ERRORS tab.'),  
    tabsetPanel(
      ##### >> 3.1. INPUTS PANEL #####
      tabPanel("INPUTS",
               fluidRow(column(3,
                               radioButtons(inputId = 'climate.available',
                                            label = "Climate data:",
                                            choices = c('Yes', 'No'))),
                        column(3,
                               radioButtons(inputId = 'vegetation.available',
                                            label = "Vegetation data:",
                                            choices = c('Yes', 'No'), selected = 'No'))),
               
               h2("Template"),
               fluidRow(column(6,
                               img(src='template-choice.png',height="100%", width="100%")),
                        column(4,
                               fluidRow(
                                 radioButtons(inputId = 'nb.sensors',
                                              label = "Number of sensors per logger:",
                                              choices = c('One sensor per logger', 'Multisensor logger')),
                                 
                                 radioButtons(inputId = 'ts.format',
                                              label = "Time series format:",
                                              choices = c('Long format', 'Separated sheets format')),
                                 
                                 radioButtons(inputId = 'file.type',
                                              label = "File format:",
                                              choices = c('XLSX', 'ODS','Parquet', 'CSV', 'TXT'))
                               )
                        )),
               fluidRow(column(3,
                               h2("Upload data"),
                               uiOutput(outputId = 'import.ui'),
                               actionButton(
                                 "loadBtn",
                                 div(
                                   id = "buttonContent", 
                                   HTML('<i class="fas fa-spinner fa-spin" id="icon"></i> Click after upload and wait loading')
                                 ),
                                 style = " margin-top: 1px; width: 250px; height: 40px; background-color: lightblue;"
                               )
               )
               ), 
      ),
      ##### >> 3.2. SUMMARY PANEL#####
      #TODO add nb sensors, sites ...
      tabPanel("SUMMARY",
               h2("Sites summary"),
               DTOutput('summary'),
               h2("Sites spatial distribution"),
               leafletOutput("map"),
               h2("Biome distribution"),
               plotOutput(outputId = 'biome'),
               h2("Sensors"),
               fluidRow(
                 column(6,
                        plotOutput(outputId = 'type')
                 ),
                 column(6,
                        plotOutput(outputId = 'height')
                 )
               ),
               h2("Sampling temporal distribution"),
               plotOutput('timeseries')
      ),
      ##### >> 3.3. TABLE PANEL #####
      tabPanel("TABLES",
               h2("Metadata table"),
               DTOutput('metadata'),
               h2("People table"),
               DTOutput('people'),
               h3("Timeseries table"),
               DTOutput('timeseries.table')
      ),
      ##### >> 3.3. ERRORS PANEL #####
      tabPanel("ERRORS",
               tags$style(HTML("
    .critical-class {
      background-color: #FFCCCC; /* Rouge */
      padding: 10px;
    }

    .major-class {
      background-color: #FFCC99; /* Orange */
      padding: 10px;
    }

    .minor-class {
      background-color: #FFFF99; /* Jaune */
      padding: 10px;
    }
  ")),
               div(class = "critical-class",uiOutput("critical")),
               div(class = "major-class",uiOutput("major")),
               div(class = "minor-class",uiOutput("minor"))
      ),
      ##### >> 3.4. DOWNLOAD PANEL #####
      tabPanel("DOWNLOAD & SUBMIT",
               h1("Summary report:"),
               downloadButton(outputId = "report", label = "Generate report"),
               h1("Download dataset:"),
               downloadButton(outputId = "save.rds",
                              label = "Download data"),
               h1("Formated dataset for submission:"),
               downloadButton(outputId = "encrypted.zip",
                              label = "Download data"),
               h1("Submit your data:"),
               HTML('
               Only when all the "Critical Errors" are resolved. <br>
                <ol>
                  <li>Download the summary report and the dataset formatted for the BEXIS2 server.</li>
                  <li>On the BEXIS submission plateform:<br>
                      <ul>
                        <li>Create a new submission</li>
                        <li>Upload the report.pdf, your original dataset, and the formatted data as attachements.</li>
                      </ul> 
                  </li>
                </ol>
                Link to BEXIS2: <a href="https://database.soilbon.org/"> https://database.soilbon.org </a>
                    ')
      )
    ))) 

#### > 4. SERVER ####
server <- function(input, output, session) {
  temp_folder <- tempdir()
  #### >> 4.1. DOWNLOAD DATA SETTINGS ####
  # The import UI will be design as function of the chosen format
  output$import.ui <- renderUI(
    if(input$file.type %in% c('XLSX', "ODS","Parquet")){
      fileInput(inputId = 'sheet.file', 
                label = 'Upload the workbook',
                multiple = F,
                accept = c('.xlsx', '.ods','.parquet'))
    }else{
      fluidRow(
        column(2,
               if(input$file.type == 'TXT'){
                 tagList(
                   radioButtons(inputId = 'column.file',
                                label = "Column separator:",
                                choices = c('tab','whitespace','dot', 'comma')),
                   radioButtons(inputId = 'dec.file',
                                label = "Decimal separator:",
                                choices = c('dot', 'comma')),
                   numericInput(inputId = 'skip.file',
                                label = 'Skip row:',
                                value = 0)
                 )
               }else{
                 tagList(
                   radioButtons(inputId = 'dec.file',
                                label = "Decimal separator:",
                                choices = c('dot', 'comma')),
                   numericInput(inputId = 'skip.file',
                                label = 'Skip:',
                                value = 0)
                 )
               }
        ),
        column(4,
               tagList(
                 fileInput(inputId = 'metadata.file', 
                           label = 'Metadata file',
                           multiple = F,
                           accept = c('.csv', '.txt')),
                 fileInput(inputId = 'people.file', 
                           label = 'People file',
                           multiple = F,
                           accept = c('.csv', '.txt')),
                 fileInput(inputId = 'timeseries.file', 
                           label = 'Time series files',
                           multiple = T,
                           accept = c('.csv', '.txt'))
               )),
        column(4,
               if(input$vegetation.available == "Yes"){
                 tagList(
                   fileInput(inputId = 'vegetation.metadata.file', 
                             label = 'Vegetation metadata file',
                             multiple = F,
                             accept = c('.csv', '.txt')),
                   fileInput(inputId = 'vegetation.data.file', 
                             label = 'Vegetation data file',
                             multiple = F,
                             accept = c('.csv', '.txt')),
                   fileInput(inputId = 'species.details.file', 
                             label = 'Species details files',
                             multiple = F,
                             accept = c('.csv', '.txt')))
               }else{
                 h5("No vegetation data to submit")
               }
        )
      )
      
    } 
  )
  
  observeEvent(input$loadBtn,{ 
    #### OPENING DATA ####
    # Opening as function of the chosen format
    if(input$file.type == 'XLSX'){
      loading.excel = function(loaded.files, vegetation.available){
        # Read the "People" sheet from the Excel file and assign the data to the variable "df.people"
        # Remove the first row from the data
        df.people = read_excel(loaded.files, sheet = "People") %>%
          filter(Email != "Stable email address") |>
          filter_all(any_vars(!is.na(.)))
        # Read the "df.metadata" sheet from the Excel file and assign the data to the variable "df.metadata"
        # Remove the first row from the data
        df.metadata = read_excel(loaded.files, sheet = "Metadata") %>% 
          filter(meta_id != "Add a unique number for each row",
                 meta_id != "Each line in this metadata-file represents one TIME SERIES, as measured by one SENSOR.") %>%
          filter_all(any_vars(!is.na(.)))
        # Remove rows in "df.metadata" that start with specific phrases
        df.metadata <- 
          df.metadata[!grepl("^IMPORTANT GENERAL INFORMATION \\[REMOVE WHEN SUBMITTING\\]:|^Each line in this df.metadata-file represents one TIME SERIES, as measured by one SENSOR\\.|^One sensor can result in multiple time series \\(e.g., if it gets redeployed elsewhere\\)\\.|^A LOGGER is a device holding multiple SENSORS, such as the TOMST TMS4 with its 4 sensors \\(3 temperature \\+ 1 soil moisture\\)\\.|^A SITE is a specific location \\(with associated latitude and longitude\\) where a measurement is done \\(e.g. a plot\\)\\.|^An EXPERIMENT is a cohesive group of sites, following the same protocol or otherwise belonging together\\.", df.metadata$meta_id), ]
        # Read the "Species details" sheet from the Excel file and assign the data to the variable "df.species"
        if(vegetation.available == 'Yes'){
          # Read the "Vegetation metadata" sheet from the Excel file and assign the data to the variable "Vegetation metadata"
          # Remove the first row from the data
          df.vegetation.metadata = read_excel(loaded.files, sheet = "Vegetation metadata")
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          # Check if the data frame has columns
          if (ncol(df.vegetation.metadata) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.metadata) > 0) {
              df.vegetation.metadata <- df.vegetation.metadata[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          #  Read the "Vegetation data" sheet from the Excel file and assign the data to the variable "Vegetation data"
          # Remove the first row from the data
          df.vegetation.data = read_excel(loaded.files, sheet = "Vegetation data")
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.vegetation.data) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.data) > 0) {
              df.vegetation.data <- df.vegetation.data[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          
          #TODO Check rows to remove without [-1,] same with others
          
          # Read the "Species details"
          df.species = read_excel(loaded.files, sheet = "Species details")
          #   [-1,] %>% 
          #     filter_all(any_vars(!is.na(.)))
          if (ncol(df.species) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.species) > 0) {
              df.vegetation.species <- df.species[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
        }
        # Remove the first row from the data
        
        if ("Raw time series data" %in% excel_sheets(loaded.files)) {
          # If the Excel file contains a sheet named "Raw time series data"
          # Read the sheet and assign the data to the variable "df_fusion"
          df.ts <- read_excel(loaded.files, sheet = "Raw time series data")
        } else {
          # Assign the values from the column 'Raw_data_identifier*' in 
          # df.metadata, excluding the first row, to the variable "feuilles"
          # Create an empty data frame named "df.ts"
          df.ts = 
            map_df(
              .x = df.metadata$`Raw_data_identifier`,
              .f = ~{
                read_excel(loaded.files, sheet = .x) |> 
                  mutate(Raw_data_identifier = .x)
              }
            )
        }
        
        colnames(df.metadata) = str_to_upper(colnames(df.metadata))
        colnames(df.people) = str_to_upper(colnames(df.people))
        colnames(df.ts) = str_to_upper(colnames(df.ts))
        colnames(df.ts)[grep('TIME', colnames(df.ts))] = 'TIME'
        ## stack the time series data into one column
        df.ts <- df.ts %>%
          pivot_longer(
            cols = -c(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME),  
            names_to = "SENSOR_ID",
            values_to = "CTS_VALUES"
          ) %>%
          mutate(
            CTS_VALUES = as.numeric(CTS_VALUES),   # b only values to numeric
            SENSOR_ID = as.character(SENSOR_ID),   # b keep sensor as character
            RAW_DATA_IDENTIFIER = as.character(RAW_DATA_IDENTIFIER)  # b keep identifier as character
          ) %>%
          arrange(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME)
        
        if (length(unique(df.ts$SENSOR_ID)) > 1) {
          df.ts <- df.ts %>%
            mutate(RAW_DATA_IDENTIFIER = paste0(RAW_DATA_IDENTIFIER, "_", SENSOR_ID))
        }
        
        
        DATA = 
          list(
            METADATA = df.metadata,
            PEOPLE = df.people,
            TIMESERIES = df.ts
          )
        
        # Compare RAW_DATA_IDENTIFIER between metadata and timeseries
        unmatched <- setdiff(df.ts$RAW_DATA_IDENTIFIER, df.metadata$RAW_DATA_IDENTIFIER)
        
        if (length(unmatched) > 0) {
          cat("??? Unmatched RAW_DATA_IDENTIFIER found:\n")
          print(unmatched)
          stop("Stopping: Some RAW_DATA_IDENTIFIER values in df.ts are missing in df.metadata")
        } else {
          cat("??? All RAW_DATA_IDENTIFIER values match between df.ts and df.metadata.\n")
        }
        
        # Check for duplicates in mts_owner_id
        dupes <- df.metadata$RAW_DATA_IDENTIFIER[duplicated(df.metadata$RAW_DATA_IDENTIFIER)]
        
        if (length(dupes) > 0) {
          cat("Duplicate values found:\n")
          print(unique(dupes))
          stop("Script stopped due to duplicates.")
        } else {
          cat("OK\n")
        }
        
        
        if(vegetation.available == 'Yes'){
          colnames(df.vegetation.metadata) = str_to_upper(colnames(df.vegetation.metadata))
          colnames(df.vegetation.data) = str_to_upper(colnames(df.vegetation.data))
          colnames(df.species) = str_to_upper(colnames(df.species))
          DATA[['VEGETATION.METADATA']] = df.vegetation.metadata
          DATA[['VEGETATION.DATA']] = df.vegetation.data
          DATA[['SPECIES.DETAILS']] = df.species
        }
        return(DATA)
      }
      DF = loading.excel(loaded.files = input$sheet.file$datapath[1],
                         vegetation.available = input$vegetation.available)
      
    }else if(input$file.type == 'ODS'){
      loading.ods = function(loaded.files, vegetation.available){
        # Read the "People" sheet from the Excel file and assign the data to the variable "df.people"
        # Remove the first row from the data
        df.people = read_ods(loaded.files, sheet = "People")%>%
          filter(Email != "Stable email address") |>
          filter_all(any_vars(!is.na(.)))
        # Read the "df.metadata" sheet from the Excel file and assign the data to the variable "df.metadata"
        # Remove the first row from the data
        df.metadata = read_ods(loaded.files, sheet = "Metadata") %>% 
          filter(meta_id != "Add a unique number for each row",
                 meta_id != "Each line in this metadata-file represents one TIME SERIES, as measured by one SENSOR.") %>%
          filter_all(any_vars(!is.na(.)))
        # Remove rows in "df.metadata" that start with specific phrases
        df.metadata <- 
          df.metadata[!grepl("^IMPORTANT GENERAL INFORMATION \\[REMOVE WHEN SUBMITTING\\]:|^Each line in this df.metadata-file represents one TIME SERIES, as measured by one SENSOR\\.|^One sensor can result in multiple time series \\(e.g., if it gets redeployed elsewhere\\)\\.|^A LOGGER is a device holding multiple SENSORS, such as the TOMST TMS4 with its 4 sensors \\(3 temperature \\+ 1 soil moisture\\)\\.|^A SITE is a specific location \\(with associated latitude and longitude\\) where a measurement is done \\(e.g. a plot\\)\\.|^An EXPERIMENT is a cohesive group of sites, following the same protocol or otherwise belonging together\\.", df.metadata$meta_id), ]
        # Read the "Species details" sheet from the Excel file and assign the data to the variable "df.species"
        if(vegetation.available == 'Yes'){
          # Read the "Vegetation metadata" sheet from the Excel file and assign the data to the variable "Vegetation metadata"
          # Remove the first row from the data
          df.vegetation.metadata = read_ods(loaded.files, sheet = "Vegetation_metadata")
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.vegetation.metadata) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.metadata) > 0) {
              df.vegetation.metadata <- df.vegetation.metadata[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          # Read the "Vegetation data" sheet from the Excel file and assign the data to the variable "Vegetation data"
          # Remove the first row from the data
          df.vegetation.data = read_ods(loaded.files, sheet = "Vegetation_data")
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          # Check if the data frame has columns
          if (ncol(df.vegetation.data) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.data) > 0) {
              df.vegetation.data <- df.vegetation.data[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          # Read the "Species details"
          df.species = read_ods(loaded.files, sheet = "Species_details")
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.species) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.species) > 0) {
              df.vegetation.species <- df.species[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
        }
        # Remove the first row from the data
        
        if("Raw_time_series_data" %in% list_ods_sheets(loaded.files)) {
          # If the Excel file contains a sheet named "Raw time series data"
          # Read the sheet and assign the data to the variable "df_fusion"
          df.ts <- read_ods(loaded.files, sheet = "Raw_time_series_data")
        }else{
          # Assign the values from the column 'Raw_data_identifier*' in 
          # df.metadata, excluding the first row, to the variable "feuilles"
          # Create an empty data frame named "df.ts"
          df.ts = 
            map_df(
              .x = df.metadata$`Raw_data_identifier`,
              .f = ~{
                read_ods(loaded.files, sheet = .x) |> 
                  mutate(Raw_data_identifier = .x)
              }
            )
        }
        
        colnames(df.metadata) = str_to_upper(colnames(df.metadata))
        colnames(df.people) = str_to_upper(colnames(df.people))
        colnames(df.ts) = str_to_upper(colnames(df.ts))
        colnames(df.ts)[grep('TIME', colnames(df.ts))] = 'TIME'
        ## stack the time series data into one column
        df.ts <- df.ts %>%
          pivot_longer(
            cols = -c(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME),  
            names_to = "SENSOR_ID",
            values_to = "CTS_VALUES"
          ) %>%
          mutate(
            CTS_VALUES = as.numeric(CTS_VALUES),   # only values to numeric
            SENSOR_ID = as.character(SENSOR_ID),   # keep sensor as character
            RAW_DATA_IDENTIFIER = as.character(RAW_DATA_IDENTIFIER)  # keep identifier as character
          ) %>%
          arrange(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME)
        
        if (length(unique(df.ts$SENSOR_ID)) > 1) {
          df.ts <- df.ts %>%
            mutate(RAW_DATA_IDENTIFIER = paste0(RAW_DATA_IDENTIFIER, "_", SENSOR_ID))
        }
        
        
        DATA = 
          list(
            METADATA = df.metadata,
            PEOPLE = df.people,
            TIMESERIES = df.ts
          )
        
        # Compare RAW_DATA_IDENTIFIER between metadata and timeseries
        unmatched <- setdiff(df.ts$RAW_DATA_IDENTIFIER, df.metadata$RAW_DATA_IDENTIFIER)
        
        if (length(unmatched) > 0) {
          cat("??? Unmatched RAW_DATA_IDENTIFIER found:\n")
          print(unmatched)
          stop("Stopping: Some RAW_DATA_IDENTIFIER values in df.ts are missing in df.metadata")
        } else {
          cat("??? All RAW_DATA_IDENTIFIER values match between df.ts and df.metadata.\n")
        }
        # Check for duplicates in mts_owner_id
        dupes <- df.metadata$RAW_DATA_IDENTIFIER[duplicated(df.metadata$RAW_DATA_IDENTIFIER)]
        
        if (length(dupes) > 0) {
          cat("Duplicate values found:\n")
          print(unique(dupes))
          stop("Script stopped due to duplicates.")
        } else {
          cat("OK\n")
        }
        
        if(vegetation.available == 'Yes'){
          colnames(df.vegetation.metadata) = str_to_upper(colnames(df.vegetation.metadata))
          colnames(df.vegetation.data) = str_to_upper(colnames(df.vegetation.data))
          colnames(df.species) = str_to_upper(colnames(df.species))
          DATA[['VEGETATION.METADATA']] = df.vegetation.metadata
          DATA[['VEGETATION.DATA']] = df.vegetation.data
          DATA[['SPECIES.DETAILS']] = df.species
        }
        return(DATA)
      }
      DF = loading.ods(loaded.files = input$sheet.file$datapath[1],
                       vegetation.available = input$vegetation.available)
    }
    
    ## here to implement parquet file format 
    else if(input$file.type == 'Parquet'){
      loading.parquet = function(loaded.files, vegetation.available){
        # Read the "People" sheet from the Excel file and assign the data to the variable "df.people"
        # Remove the first row from the data
        df.people = arrow::read_parquet(loaded.files) %>%
          filter(SheetName == "People")%>%
          filter(!if_all(everything(), is.na))%>%
          dplyr:: select(where(~ !all(is.na(.))))%>%
          filter(Email != "Stable email address") |>
          filter_all(any_vars(!is.na(.)))
        
        
        # Read the "df.metadata" sheet from the Excel file and assign the data to the variable "df.metadata"
        # Remove the first row from the data
        df.metadata <- arrow::read_parquet(loaded.files) %>%
          slice(-1) %>%                               # Exclude the first row
          filter(SheetName == "Metadata") %>%         # Keep only rows where SheetName is 'Metadata'
          filter(!if_all(everything(), is.na)) %>%    # Remove rows where all columns are NA
          dplyr::select(where(~ !all(is.na(.)))) %>%         # Keep only columns that are not entirely NA
          dplyr::select(-SheetName) %>%                      # Exclude the 'SheetName' column
          filter(if_any(everything(), ~ !is.na(.)))   # Keep rows with at least one non-NA value
        
        # 
        # Remove rows in "df.metadata" that start with specific phrases
        # df.metadata <- 
        #   df.metadata[!grepl("^IMPORTANT GENERAL INFORMATION \\[REMOVE WHEN SUBMITTING\\]:|^Each line in this df.metadata-file represents one TIME SERIES, as measured by one SENSOR\\.|^One sensor can result in multiple time series \\(e.g., if it gets redeployed elsewhere\\)\\.|^A LOGGER is a device holding multiple SENSORS, such as the TOMST TMS4 with its 4 sensors \\(3 temperature \\+ 1 soil moisture\\)\\.|^A SITE is a specific location \\(with associated latitude and longitude\\) where a measurement is done \\(e.g. a plot\\)\\.|^An EXPERIMENT is a cohesive group of sites, following the same protocol or otherwise belonging together\\.", df.metadata$meta_id), ]
        # Read the "Species details" sheet from the Excel file and assign the data to the variable "df.species"
        if(vegetation.available == 'Yes'){
          # Read the "Vegetation metadata" sheet from the Excel file and assign the data to the variable "Vegetation metadata"
          # Remove the first row from the data
          df.vegetation.metadata <- arrow::read_parquet(loaded.files) %>%
            filter(SheetName == "Vegetation metadata")%>%
            filter(!if_all(everything(), is.na))%>%
            dplyr:: select(where(~ !all(is.na(.))))%>%
            dplyr::select(-SheetName) %>%                      # Exclude the 'SheetName' column
            filter(if_any(everything(), ~ !is.na(.)))   # Keep rows with at least one non-NA value
          
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          # Check if the data frame has columns
          if (ncol(df.vegetation.metadata) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.metadata) > 0) {
              df.vegetation.metadata <- df.vegetation.metadata[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          #  Read the "Vegetation data" sheet from the Excel file and assign the data to the variable "Vegetation data"
          # Remove the first row from the data
          
          
          
          df.vegetation.data <- arrow::read_parquet(loaded.files) %>%
            filter(SheetName == "Vegetation data")%>%
            filter(!if_all(everything(), is.na))%>%
            dplyr:: select(where(~ !all(is.na(.))))%>%
            dplyr::select(-SheetName)  # Exclude the 'SheetName' colum 
          # [-1,] %>% 
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.vegetation.data) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.data) > 0) {
              df.vegetation.data <- df.vegetation.data[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          
          # # TODO Check rows to remove without [-1,] same with others
          
          # Read the "Species details"
          if ("species details" %in% unique(arrow::read_parquet(loaded.files)$SheetName)) {
            df.species <- arrow::read_parquet(loaded.files) %>%
              filter(SheetName == "species details") %>%
              filter(!if_all(everything(), is.na)) %>%
              dplyr::select(where(~ !all(is.na(.)))) %>%
              dplyr::select(-SheetName)  # Exclude the 'SheetName' column
          } else {
            # If "species details" is not found, return an empty data frame
            df.species <- data.frame()
          }
          #   [-1,] %>% 
          #     filter_all(any_vars(!is.na(.)))
          if (ncol(df.species) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.species) > 0) {
              df.vegetation.species <- df.species[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
        }
        # Remove the first row from the data
        
        # Check if the "Raw time series" or "Raw time series data" exists in the SheetName column
        if ("Raw time series" %in% arrow::read_parquet(loaded.files)$SheetName | 
            "Raw time series data" %in% arrow::read_parquet(loaded.files)$SheetName) {
          
          # Read the data and filter for "Raw time series" and "Raw time series data"
          df.ts <- arrow::read_parquet(loaded.files) %>%
            # mutate(SheetName = ifelse("SheetName" %in% colnames(.), SheetName, NA_character_)) %>%  # Ensure SheetName exists
            filter(SheetName %in% c("Raw time series", "Raw time series data")) %>%  # Keep relevant sheets
            filter(!if_all(everything(), is.na)) %>%  # Remove completely empty rows
            dplyr::select(where(~ !all(is.na(.)))) %>%  # Remove empty columns
            dplyr::select(-SheetName)  # Drop SheetName column
        } else {
          # Assign the values from the column 'Raw_data_identifier*' in 
          # df.metadata, excluding the first row, to the variable "feuilles"
          # Create an empty data frame named "df.ts"
          df.ts = 
            map_df(
              .x = df.metadata$`Raw_data_identifier`,
              .f = ~{
                read_parquet(loaded.files, sheet = .x) |> 
                  mutate(Raw_data_identifier = .x)
              }
            )
        }
        
        colnames(df.metadata) = str_to_upper(colnames(df.metadata))
        colnames(df.people) = str_to_upper(colnames(df.people))
        colnames(df.ts) = str_to_upper(colnames(df.ts))
        colnames(df.ts)[grep('TIME', colnames(df.ts))] = 'TIME'
        ## stack the time series data into one column
        df.ts <- df.ts %>%
          pivot_longer(
            cols = -c(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME),  
            names_to = "SENSOR_ID",
            values_to = "CTS_VALUES"
          ) %>%
          mutate(
            CTS_VALUES = as.numeric(CTS_VALUES),   # b only values to numeric
            SENSOR_ID = as.character(SENSOR_ID),   # b keep sensor as character
            RAW_DATA_IDENTIFIER = as.character(RAW_DATA_IDENTIFIER)  # b keep identifier as character
          ) %>%
          arrange(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME)
        
        if (length(unique(df.ts$SENSOR_ID)) > 1) {
          df.ts <- df.ts %>%
            mutate(RAW_DATA_IDENTIFIER = paste0(RAW_DATA_IDENTIFIER, "_", SENSOR_ID))
        }
        
        
        DATA = 
          list(
            METADATA = df.metadata,
            PEOPLE = df.people,
            TIMESERIES = df.ts
          )
        
        # Compare RAW_DATA_IDENTIFIER between metadata and timeseries
        unmatched <- setdiff(df.ts$RAW_DATA_IDENTIFIER, df.metadata$RAW_DATA_IDENTIFIER)
        
        if (length(unmatched) > 0) {
          cat("??? Unmatched RAW_DATA_IDENTIFIER found:\n")
          print(unmatched)
          stop("Stopping: Some RAW_DATA_IDENTIFIER values in df.ts are missing in df.metadata")
        } else {
          cat("??? All RAW_DATA_IDENTIFIER values match between df.ts and df.metadata.\n")
        }
        
        # Check for duplicates in mts_owner_id
        dupes <- df.metadata$RAW_DATA_IDENTIFIER[duplicated(df.metadata$RAW_DATA_IDENTIFIER)]
        
        if (length(dupes) > 0) {
          cat("Duplicate values found:\n")
          print(unique(dupes))
          stop("Script stopped due to duplicates.")
        } else {
          cat("OK\n")
        }
        
        
        
        if(vegetation.available == 'Yes'){
          colnames(df.vegetation.metadata) = str_to_upper(colnames(df.vegetation.metadata))
          colnames(df.vegetation.data) = str_to_upper(colnames(df.vegetation.data))
          colnames(df.species) = str_to_upper(colnames(df.species))
          DATA[['VEGETATION.METADATA']] = df.vegetation.metadata
          DATA[['VEGETATION.DATA']] = df.vegetation.data
          DATA[['SPECIES.DETAILS']] = df.species
        }
        return(DATA)
      }
      DF = loading.parquet(loaded.files = input$sheet.file$datapath[1],
                           vegetation.available = input$vegetation.available)
      
    }else if(input$file.type == 'CSV'){
      loading.csv = function(metadata.file,
                             people.file,
                             timeseries.files,
                             vegetation.available,
                             vegetation.metadata.file = NULL,
                             vegetation.data.file = NULL,
                             species.details.file = NULL){
        # Read the "People" sheet from the Excel file and assign the data to the variable "df.people"
        # Remove the first row from the data
        df.people = read_csv(people.file)[-1,] %>%
          filter_all(any_vars(!is.na(.)))
        # Read the "df.metadata" sheet from the Excel file and assign the data to the variable "df.metadata"
        # Remove the first row from the data
        df.metadata = read_csv(metadata.file)[-1,] %>%
          filter_all(any_vars(!is.na(.)))
        # Remove rows in "df.metadata" that start with specific phrases
        df.metadata <-
          df.metadata[!grepl("^IMPORTANT GENERAL INFORMATION \\[REMOVE WHEN SUBMITTING\\]:|^Each line in this df.metadata-file represents one TIME SERIES, as measured by one SENSOR\\.|^One sensor can result in multiple time series \\(e.g., if it gets redeployed elsewhere\\)\\.|^A LOGGER is a device holding multiple SENSORS, such as the TOMST TMS4 with its 4 sensors \\(3 temperature \\+ 1 soil moisture\\)\\.|^A SITE is a specific location \\(with associated latitude and longitude\\) where a measurement is done \\(e.g. a plot\\)\\.|^An EXPERIMENT is a cohesive group of sites, following the same protocol or otherwise belonging together\\.", df.metadata$meta_id), ]
        # Read the "Species details" sheet from the Excel file and assign the data to the variable "df.species"
        if(vegetation.available == 'Yes'){
          # Read the "Vegetation metadata" sheet from the Excel file and assign the data to the variable "Vegetation metadata"
          # Remove the first row from the data
          df.vegetation.metadata = read_csv(vegetation.metadata.file)
          # [-1,] %>%
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.vegetation.metadata) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.metadata) > 0) {
              df.vegetation.metadata <- df.vegetation.metadata[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          # # Read the "Vegetation data" sheet from the Excel file and assign the data to the variable "Vegetation data"
          # Remove the first row from the data
          df.vegetation.data = read_csv(vegatation.data.file)
          # [-1,] %>%
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.vegetation.data) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.vegetation.data) > 0) {
              df.vegetation.data <- df.vegetation.data[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
          # Read the "Species details"
          df.species = read_csv(SPECIES.DETAILS.file)
          # [-1,] %>%
          #   filter_all(any_vars(!is.na(.)))
          if (ncol(df.species) > 0) {
            # Remove the first row and filter out rows with all NA values, only if there are rows in the data frame
            if (nrow(df.species) > 0) {
              df.vegetation.species <- df.species[-1, ] %>% filter_all(any_vars(!is.na(.)))
            } else {
              cat("Data frame has no rows.\n")
            }
          } else {
            cat("Data frame has no columns.\n")
          }
        }
        # Remove the first row from the data
        if(length(timeseries.files) == 1) {
          # If the Excel file contains a sheet named "Raw time series data"
          # Read the sheet and assign the data to the variable "df_fusion"
          df.ts <- read_csv(timeseries.files)
        } else {
          df.ts =
            map_df(
              .x = df.metadata$`Raw_data_identifier`,
              .f = ~{
                read_csv(.x) |>
                  mutate(Raw_data_identifier = .x)
              }
            )
        }
        colnames(df.metadata) = str_to_upper(colnames(df.metadata))
        colnames(df.people) = str_to_upper(colnames(df.people))
        colnames(df.ts) = str_to_upper(colnames(df.ts))
        colnames(df.ts)[grep('TIME', colnames(df.ts))] = 'TIME'
        ## stack the time series data into one column
        df.ts <- df.ts %>%
          pivot_longer(
            cols = -c(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME),  
            names_to = "SENSOR_ID",
            values_to = "CTS_VALUES"
          ) %>%
          mutate(
            CTS_VALUES = as.numeric(CTS_VALUES),   #  only values to numeric
            SENSOR_ID = as.character(SENSOR_ID),   # keep sensor as character
            RAW_DATA_IDENTIFIER = as.character(RAW_DATA_IDENTIFIER)  # keep identifier as character
          ) %>%
          arrange(RAW_DATA_IDENTIFIER, YEAR, MONTH, DAY, TIME)
        
        if (length(unique(df.ts$SENSOR_ID)) > 1) {
          df.ts <- df.ts %>%
            mutate(RAW_DATA_IDENTIFIER = paste0(RAW_DATA_IDENTIFIER, "_", SENSOR_ID))
        }
        
        
        DATA = 
          list(
            METADATA = df.metadata,
            PEOPLE = df.people,
            TIMESERIES = df.ts
          )
        
        # Compare RAW_DATA_IDENTIFIER between metadata and timeseries
        unmatched <- setdiff(df.ts$RAW_DATA_IDENTIFIER, df.metadata$RAW_DATA_IDENTIFIER)
        
        if (length(unmatched) > 0) {
          cat("??? Unmatched RAW_DATA_IDENTIFIER found:\n")
          print(unmatched)
          stop("Stopping: Some RAW_DATA_IDENTIFIER values in df.ts are missing in df.metadata")
        } else {
          cat("??? All RAW_DATA_IDENTIFIER values match between df.ts and df.metadata.\n")
        }
        
        # Check for duplicates in mts_owner_id
        dupes <- df.metadata$RAW_DATA_IDENTIFIER[duplicated(df.metadata$RAW_DATA_IDENTIFIER)]
        
        if (length(dupes) > 0) {
          cat("Duplicate values found:\n")
          print(unique(dupes))
          stop("Script stopped due to duplicates.")
        } else {
          cat("OK\n")
        }
        
        if(vegetation.available == 'Yes'){
          colnames(df.vegetation.metadata) = str_to_upper(colnames(df.vegetation.metadata))
          colnames(df.vegetation.data) = str_to_upper(colnames(df.vegetation.data))
          colnames(df.species) = str_to_upper(colnames(df.species))
          DATA[['VEGETATION.METADATA']] = df.vegetation.metadata
          DATA[['VEGETATION.DATA']] = df.vegetation.data
          DATA[['SPECIES.DETAILS']] = df.species
        }
        return(DATA)
      }
      if(input$vegetation.available == 'Yes'){
        DF = loading.csv(metadata.file = input$metadata.file$datapath[1],
                         people.file = input$people.file$datapath[1],
                         timeseries.files = input$timeseries.file$datapath[1],
                         vegetation.available = input$vegetation.available,
                         vegetation.metadata.file  = input$vegtation.metadata.file$datapath[1],
                         vegetation.data.file = input$vegtation.data.file$datapath[1],
                         species.details.file = input$species.details.file$datapath[1])
      }else if(input$vegetation.available == 'No'){
        DF = loading.csv(metadata.file = input$metadata.file$datapath[1],
                         people.file = input$people.file$datapath[1],
                         timeseries.files = input$timeseries.file$datapath[1],
                         vegetation.available = input$vegetation.available,
                         vegetation.metadata.file = NULL,
                         vegetation.data.file = NULL,
                         species.details.file = NULL)
      }
    }else{
      print("NOT IMPLEMENTED YET") #TODO @Bedassa implement TXT inputs
    }
    
    #TODO @Bedassa here is where you could aim for some reformatting 
    # of DF when needed
    
    #### >> 4.2. DATA CONTROL ####
    #### >>> 4.2.1 TABLES ####
    # Check for missing tables
    for(i in names(L.ERRORS$Comptabilty$Open_sheet)){
      L.ERRORS$Comptabilty$Open_sheet[[i]] =
        if_else(is.data.frame(DF[[i]]), 'Available', 'Missing')
    }
    
    #### >>> 4.2.2 COLUMNS ####
    # Check added columns
    columns.to.check = names(DF)
    for(i in columns.to.check[columns.to.check!="TIMESERIES"]){
      for(j in colnames(DF[[i]])){
        if(!(j %in% df.columns$Column[df.columns$Sheet == i])){
          L.ERRORS$Structure$NameColumn[[i]]$Added = 
            c(L.ERRORS$Structure$NameColumn[[i]]$Added, i)
        }
      }
    }
    
    a = df.columns[df.columns$Sheet %in% names(DF),]
    # Check and add missing columns
    for(i in 1:nrow(a)){
      if(!(a$Column[i] %in% colnames(DF[[a$Sheet[i]]]))){
        L.ERRORS$Structure$NameColumn[[a$Sheet[i]]]$Missing = 
          c(L.ERRORS$Structure$NameColumn[[a$Sheet[i]]]$Missing, 
            a$Column[i])
        DF[[a$Sheet[i]]][,a$Column[i]] = NA
      }
    }
    
    #### >>> 4.2.3 VALUES ####
    L.ERRORS = check.all.errors(DATAFRAMES = DF, 
                                L.ERRORS = L.ERRORS, 
                                vegetation.available = input$vegetation.available)
    
    #### >>> 4.2.4 COMBINE ERRORS ####
    C.METADATA = 
      extract.errors(LIST = L.ERRORS$Format$METADATA,
                     level = 'critical')
    C.PEOPLE = 
      extract.errors(LIST = L.ERRORS$Format$PEOPLE,
                     level = 'critical')
    
    Ma.METADATA = 
      extract.errors(LIST = L.ERRORS$Format$METADATA,
                     level = 'major')
    Ma.PEOPLE = 
      extract.errors(LIST = L.ERRORS$Format$PEOPLE,
                     level = 'major')
    
    Mi.METADATA = 
      extract.errors(LIST = L.ERRORS$Format$METADATA,
                     level = 'minor')
    Mi.PEOPLE = 
      extract.errors(LIST = L.ERRORS$Format$PEOPLE,
                     level = 'minor')
    ## To add the critical, major and minor errors to report data frame 
    report<-NULL
    report$METADTA <- NULL
    report$PEOPLE <-NULL
    
    report$METADATA$Critical <- C.METADATA 
    report$METADATA$Major <-Ma.METADATA
    report$METADATA$Minor <-Mi.METADATA
    
    report$PEOPLE$Critical <- C.PEOPLE 
    report$PEOPLE$Major <-Ma.PEOPLE
    report$PEOPLE$Minor <-Mi.PEOPLE
    
    
    #### >> 4.3. GENERATE SUMMARY PAGE ####
    ##### Map #####
    output$summary = 
      renderDT({
        data.frame(
          `Unique sites` = length(unique(paste0(DF$METADATA$LONGITUDE, '-', DF$METADATA$LATITUDE))), 
          `Number of timeseries` = nrow(DF$METADATA),
          `Microclimate variables` = paste0(unique(DF$METADATA$MICROCLIMATE_MEASUREMENT), collapse = ', ')
        ) |> 
          t()
      })
    output$map <- renderLeaflet({
      blueCursorIcon <- makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
        iconWidth = 35, iconHeight = 55
      )
      redCursorIcon <- makeIcon(
        iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
        iconWidth = 35, iconHeight = 55
      )
      mymap <- isolate(leaflet(DF$METADATA)) %>%
        addTiles() %>%
        addMarkers(
          lat = ~as.numeric(LATITUDE),
          lng = ~as.numeric(LONGITUDE),
          popup = paste("Registered country code : ", 
                        DF$METADATA$COUNTRY_CODE , 
                        "<br> Site id  : ", 
                        DF$METADATA$SITE_ID)
        )
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Transform df to the same projection as the map (Mollweide)
      df_sf <- st_as_sf(DF$METADATA, 
                        coords = c("LONGITUDE", "LATITUDE"), 
                        crs = 4326)  # Assume original CRS is WGS84 (EPSG: 4326)
      df_moll <- st_transform(df_sf, crs = "+proj=moll")  # Transform to Mollweide
      
      # Extract transformed coordinates for plotting
      df_moll_coords <- as.data.frame(st_coordinates(df_moll))
      df_moll$X <- df_moll_coords$X
      df_moll$Y <- df_moll_coords$Y
      
      # Plot with transformed coordinates
      p.map <- ggplot(data = st_transform(world, crs = "+proj=moll")) +
        geom_sf(data = st_transform(world, crs = "+proj=moll"),
                alpha = 0.5) +
        geom_sf(data = st_transform(world |> 
                                      filter(geounit %in% c('Greenland', 'Antarctica')), 
                                    crs = "+proj=moll"),
                fill = '#C8D7E4')  + 
        geom_point(data = df_moll,
                   aes(x = X, y = Y),
                   alpha = 1,
                   size = 5, 
                   color = '#0471DB') +
        theme_void() + 
        theme(legend.position = 'bottom',
              legend.text = element_text(angle = 45, hjust = .5, vjust = .5)) +
        coord_sf(crs = "+proj=moll")  # Apply Mollweide projection
      
      ggsave(p.map, file = paste0(temp_folder,"/map.png"))
      mymap
    })
    ##### Biome spread #####
    output$biome <- renderPlot({
      path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
      temp_pp <- raster::stack(path)
      names(temp_pp) <- c("temperature", "precipitation")
      DF$METADATA = 
        DF$METADATA |> 
        mutate(Longitude = as.numeric(LONGITUDE)) |> 
        mutate(Latitude = as.numeric(LATITUDE)) |> 
        filter(!(is.na(Longitude)) & !is.na(Latitude))
      
      my.sf.point <- st_as_sf(x = DF$METADATA, 
                              coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      ext <- raster::extract(temp_pp, my.sf.point, df = TRUE)
      ext$temperature <- ext$temperature/10
      ext$precipitation <- ext$precipitation/10
      
      data(wrld_simpl)
      wrld_simpl <- wrld_simpl[wrld_simpl$NAME != "Antarctica", ]
      points <- sp::spsample(x = wrld_simpl, n = 10000, type = "random")
      extractions <- raster::extract(temp_pp, points, df = TRUE)
      extractions$temperature <- extractions$temperature/10
      extractions$precipitation <- extractions$precipitation/10
      
      p.whitteker = 
        whittaker_base_plot() +
        geom_point(data = extractions,
                   aes(x = temperature,
                       y = precipitation),
                   size = .1, alpha = .1) + 
        geom_point(data = ext,
                   aes(x = temperature,
                       y = precipitation),
                   alpha = 1,
                   size = 5, 
                   color = '#0471DB') +
        geom_polygon(data = Whittaker_biomes,
                     aes(x    = temp_c,
                         y    = precp_cm,
                         fill = biome),
                     # adjust polygon borders
                     colour = "gray98",
                     alpha = 0,
                     size = 1) +
        lims(y = c(0,500)) +
        labs(y = 'Precipitation (cm)') +
        theme_bw()
      ggsave(paste0(temp_folder, '/biomes.png'), 
             p.whitteker, 
             height = 10, width = 17, units = 'cm')
      p.whitteker
    })
    ##### Sensor type #####
    output$type <- renderPlot({
      df.1 <- DF$METADATA %>% 
        group_by(MICROCLIMATE_MEASUREMENT) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = paste0(round(perc*100,digits = 2), '%'))
      
      p.pie =
        ggplot(df.1, aes(x = "", y = perc, fill = MICROCLIMATE_MEASUREMENT)) +
        geom_col(alpha = 0.5) +
        geom_text(aes(label = labels),
                  position = position_stack(vjust = .5)) +
        coord_polar(theta = "y") + 
        scale_fill_viridis_d() +
        labs(fill = 'Microclimate\nvariable') + 
        theme_void() +
        theme(legend.position = "top")
      ggsave(paste0(temp_folder, '/type.png'), 
             p.pie,
             height = 10, width = 10, units = 'cm')
      p.pie
    })
    ##### Sensor height #####
    output$height <- renderPlot({
      p.height = 
        ggplot(data = DF$METADATA,
               aes(
                 y = as.numeric(SENSOR_HEIGHT), 
                 fill = MICROCLIMATE_MEASUREMENT)) +
        geom_histogram(data = DF$METADATA,
                       aes(
                         y = as.numeric(SENSOR_HEIGHT), 
                         fill = MICROCLIMATE_MEASUREMENT)) +
        labs(y = 'Sensor height (cm)', x = 'Number of sensors') + 
        scale_fill_viridis_d() +
        theme_bw() + 
        theme(legend.position = 'bottom')
      
      ggsave(paste0(temp_folder, '/height.png'), 
             p.height,
             height = 10, width = 20, units = 'cm')
      
      p.height
    })
    
    ##### Timeseries ####
    DF$TIMESERIES$DATE = ymd(paste0(DF$TIMESERIES$YEAR,'-',
                                    DF$TIMESERIES$MONTH,'-',
                                    DF$TIMESERIES$DAY))
    #### Timeseries for vegetation 
    DF$VEGETATION.METADATA$DATE = ymd(paste0(DF$VEGETATION.METADATA$OBSERVATION_DATE_YEAR,'-',
                                             DF$VEGETATION.METADATA$OBSERVATION_DATE_MONTH,'-',
                                             DF$VEGETATION.METADATA$OBSERVATION_DATE_DAY))
    # FIXME Need to check the date build -> errors in parse
    DF$TIMESERIES =
      DF$TIMESERIES |>
      dplyr::mutate(RAW_DATA_IDENTIFIER = as.character(RAW_DATA_IDENTIFIER)) |>
      left_join(DF$METADATA |>
                  dplyr::select(`RAW_DATA_IDENTIFIER`,
                         MICROCLIMATE_MEASUREMENT)|>
                  dplyr::mutate(RAW_DATA_IDENTIFIER = as.character(RAW_DATA_IDENTIFIER)))
    # 
    output$timeseries <- renderPlot({
      p.ts = 
        ggplot(data = DF$TIMESERIES,
               aes(x = as.POSIXct(DATE),
                   y = as.numeric(CTS_VALUES),
                   color = as.character(`RAW_DATA_IDENTIFIER`))) +
        geom_line(alpha = .5, size = .1) + 
        labs(x = "Date", y = "Values") + 
        theme_bw() +
        scale_x_datetime(date_labels = "%Y.%m") + 
        facet_grid(rows = vars(MICROCLIMATE_MEASUREMENT), 
                   scales = 'free') +
        theme(legend.position = 'none')
      
      ggsave(paste0(temp_folder, '/ts.png'), 
             p.ts,
             height = 7.5, width = 15, units = 'cm')
      
      p.ts
    })
    
    #### >> 4.4. GENERATE TABLES ####
    output$metadata = renderDT(DF$METADATA)
    output$people = renderDT(DF$PEOPLE)
    output$timeseries.table = renderDT(DF$TIMESERIES)
    
    #### >> 4.4. GENERATE ERROR PAGE ####
    ##### Critical #####
    output$critical = 
      renderUI(
        tagList(
          h2('CRITICAL'),
          fluidRow(
            column(6, 
                   tags$h2("Metadata"),
                   tags$h3("Missing data"),
                   renderText(
                     if(length(C.METADATA$MISSING)>0){paste0(C.METADATA$MISSING, 
                                                             collapse = ', ')
                     }else{"No missing columns"}),
                   tags$h3("Errors"),
                   if(length(C.METADATA$ERRORS)>1){
                     map(.x = C.METADATA$ERRORS,
                         .f = ~{
                           list(tags$h4(colnames(.x[[1]])[2]),
                                renderTable(.x[[1]]))
                         })
                   }else{renderText('No error spotted')}),
            column(6,
                   tags$h2("People"),
                   tags$h3("Missing data"),
                   renderText(
                     if(length(C.PEOPLE$MISSING)>0){paste0(C.PEOPLE$MISSING, 
                                                           collapse = ', ')
                     }else{"No missing columns"}),
                   tags$h3("Errors"),
                   if(length(C.PEOPLE$ERRORS)>1){
                     map(.x = C.PEOPLE$ERRORS,
                         .f = ~{
                           list(tags$h4(colnames(.x[[1]])[2]),
                                renderTable(.x[[1]]))
                         })
                   }else{renderText('No error spotted')}
            )))
      )
    ##### Major #####
    output$major = 
      renderUI(
        tagList(
          h2('MAJOR'),
          fluidRow(
            column(6, 
                   tags$h2("Metadata"),
                   tags$h3("Missing data"),
                   renderText(
                     if(length(Ma.METADATA$MISSING)>0){paste0(Ma.METADATA$MISSING, collapse = ', ')
                     }else{"No missing columns"}),
                   tags$h3("Errors"),
                   if(length(Ma.METADATA$ERRORS)>1){
                     map(.x = Ma.METADATA$ERRORS,
                         .f = ~{
                           list(tags$h4(colnames(.x[[1]])[2]),
                                renderTable(.x[[1]]))
                         })
                   }else{renderText('No error spotted')}),
            column(6,
                   tags$h2("People"),
                   tags$h3("Missing data"),
                   renderText(
                     if(length(Ma.PEOPLE$MISSING)>0){paste0(Ma.PEOPLE$MISSING, collapse = ', ')
                     }else{"No missing columns"}),
                   tags$h3("Errors"),
                   if(length(Ma.PEOPLE$ERRORS)>1){
                     map(.x = Ma.PEOPLE$ERRORS,
                         .f = ~{
                           list(tags$h4(colnames(.x[[1]])[2]),
                                renderTable(.x[[1]]))
                         })
                   }else{renderText('No error spotted')}
            )))
      )
    
    ##### Minor #####
    output$minor = 
      renderUI(
        tagList(
          h2('MINOR'),
          fluidRow(
            column(6, 
                   tags$h2("Metadata"),
                   tags$h3("Missing data"),
                   renderText(
                     if(length(Mi.METADATA$MISSING)>0){paste0(Mi.METADATA$MISSING, collapse = ', ')
                     }else{"No missing columns"}),
                   tags$h3("Errors"),
                   if(length(Mi.METADATA$ERRORS)>1){
                     map(.x = Mi.METADATA$ERRORS,
                         .f = ~{
                           list(tags$h4(colnames(.x[[1]])[2]),
                                renderTable(.x[[1]]))
                         })
                   }else{renderText('No error spotted')}),
            column(6,
                   tags$h2("People"),
                   tags$h3("Missing data"),
                   renderText(
                     if(length(Mi.PEOPLE$MISSING)>0){paste0(Mi.PEOPLE$MISSING, collapse = ', ')
                     }else{"No missing columns"}),
                   tags$h3("Errors"),
                   if(length(Mi.PEOPLE$ERRORS)>1){
                     map(.x = Mi.PEOPLE$ERRORS,
                         .f = ~{
                           list(tags$h4(colnames(.x[[1]])[2]),
                                renderTable(.x[[1]]))
                         })
                   }else{renderText('No error spotted')}
            )))
      )
    
    # TODO Add vegetation errors to critical, major and minor levels
    
    #### >> 4.5. DOWNLOAD ####
    ##### >>> 4.5.1. Datasets #####
    FULL.OUTPUT = 
      list(
        DATA = DF,
        REPORT = report,
        ERRORS = L.ERRORS
      )
    #Extract the file name from the uploaded file
    file_name = str_remove_all(tools::file_path_sans_ext(basename(input$sheet.file$name)), ' ')
    passphrase <- charToRaw("MDB secret code phrase")
    key <- sha256(passphrase)
    x <- serialize(FULL.OUTPUT, NULL)
    encrypted_x <- aes_cbc_encrypt(x, key = key)
    rds_path <- file.path(temp_folder, "dataset.rds")
    zip_path <- file.path(temp_folder, "data.zip")
    saveRDS(encrypted_x, rds_path)
    zip(zipfile = zip_path, 
        files = c(rds_path, 
                  input$sheet.file$datapath[1]))
    hash = create_hashFile(paste0(zip_path))
    
    # Define the download handler
    output$save.rds = downloadHandler(
      filename = function(){paste0(file_name, "-dataset.rds")},
      content = function(file) {
        saveRDS(FULL.OUTPUT, file = file)
      }
    )
    
    output$encrypted.zip <-  downloadHandler(
      filename = function() {
        paste0(file_name, "-BEXIS.zip")
      },
      content = function(fname) {
        # Temp folder and clean filename
        temp_folder <- tempdir()
        rds_filename <- "dataset.rds"
        rds_path <- file.path(temp_folder, rds_filename)
        
        # Save encrypted RDS
        saveRDS(encrypted_x, rds_path)
        
        # Zip only the filename (not full path)
        old_wd <- setwd(temp_folder)
        zip(zipfile = fname, files = rds_filename)
        setwd(old_wd)
      },
      contentType = "application/zip"
    )
    ##### >>> 4.5.2. Report #####
    # output$report <- downloadHandler(
    #   filename = function() {paste(file_name,"-report.pdf")},
    #   content = function(file) {
    #     # Copy the report file to a temporary directory before processing it, in
    #     # case we don't have write permissions to the current working dir (which
    #     # can happen when deployed).
    #     tempReport <- file.path(temp_folder, "report.Rmd")
    #     file.copy("report.Rmd", 
    #               tempReport, 
    #               overwrite = TRUE)
    #     # Set up parameters to pass to Rmd document
    #     params <- list(DF = DF, 
    #                    hash = hash, 
    #                    ERRORS = L.ERRORS,
    #                    report = report)
    #     
    #     # Knit the document, passing in the `params` list, and eval it in a
    #     # child of the global environment (this isolates the code in the document
    #     # from the code in this app).
    #     rmarkdown::render(tempReport, 
    #                       params = params,
    #                       output_file = file,
    #                       output_format = rmarkdown::pdf_document(),
    #                       envir = new.env(parent = globalenv()),
    #                       knit_root_dir = temp_folder
    #     )
    #   }
    # )
    # output$report <- downloadHandler(
    #   filename = function() {paste(file_name, "-report.pdf")},
    #   content = function(file) {
    #     # Copy the report file to a temporary directory before processing it,
    #     # in case we don't have write permissions to the current working dir
    #     tempReport <- file.path(tempdir(), "report.Rmd")
    #     file.copy("report.Rmd", tempReport, overwrite = TRUE)  # Ensure path is correct
    #     
    #     
    #     # Set up parameters to pass to the Rmd document
    #     params <- list(DF = DF, 
    #                    hash = hash, 
    #                    ERRORS = L.ERRORS,
    #                    report = report)
    #     
    #     # Render the Rmd file to generate a LaTeX file (.tex)
    #     rmarkdown::render(tempReport, 
    #                       params = params,
    #                       output_file = file,
    #                       output_format = rmarkdown::pdf_document(),
    #                       envir = new.env(parent = globalenv()),
    #                       knit_root_dir = temp_folder
    #     )
    #   }
    # )
    output$report <- downloadHandler(
      filename = function() {paste(file_name, "-report.pdf")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it,
        # in case we don't have write permissions to the current working dir
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)  # Ensure path is correct
        
        
        # Set up parameters to pass to the Rmd document
        params <- list(DF = DF, 
                       hash = hash, 
                       ERRORS = L.ERRORS,
                       report = report)
        
        # Render the Rmd file to generate a LaTeX file (.tex)
        rmarkdown::render(
          input = tempReport, 
          output_file = file.path(tempdir(), "file_output.tex"), # Temporary .tex file
         params = params,
          output_format = rmarkdown::pdf_document(
            latex_engine = "pdflatex",
            keep_tex = TRUE
          ),
          envir = new.env(parent = globalenv())
        )
        
        # Define path to the generated LaTeX file
        tex_file_path <- file.path(tempdir(), "file_output.tex")
        Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Program Files/MiKTeX/miktex/bin/x64", sep = ";"))
        
        # Use normalizePath to ensure correct absolute path handling
        tex_file_path <- normalizePath(tex_file_path)
        tempdir_path <- normalizePath(tempdir())  # Ensure temp directory is also fully qualified
        
        # Use double quotes around file paths to handle spaces correctly
        latex_command <- paste('"C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe"', shQuote(tex_file_path), "--output-directory", shQuote(tempdir_path))
        
        # Log the LaTeX command for debugging
        cat("Running LaTeX command:\n", latex_command, "\n")
        
        # Use system() to compile the LaTeX file with pdflatex
        latex_output <- system(latex_command, intern = TRUE)
        
        # Check if the LaTeX compilation was successful
        if (length(latex_output) > 0) {
          cat("LaTeX output:\n", latex_output, "\n")
        }
        
        # Define path to the compiled PDF file
        compiled_pdf_path <- file.path(tempdir(), "file_output.pdf")
        
        # If LaTeX compilation is successful, move the PDF to the output file location
        if (file.exists(compiled_pdf_path)) {
          file.rename(compiled_pdf_path, file)
        } else {
          warning("PDF file not generated!")
        }
        
        # Optionally, check LaTeX log file for errors
        log_file <- file.path(tempdir(), "file_output.log")
        if (file.exists(log_file)) {
          log_contents <- readLines(log_file)
          cat("LaTeX Log:\n", log_contents, "\n")
        }
      }
    )
    
    reactive({
      req(input$file.type)  # Ensure the file is uploaded
      Sys.sleep(2)      # Simulate file processing time
      (input$file.type$datapath)  # Read the uploaded file
    })
    observe({
      req(input$file.type)  # Ensure the file is selected
      updateActionButton(session, "loadBtn", label = "Loaded")
      session$sendCustomMessage(type = 'setButtonColor', message = list(color = 'green'))
    }) 
  }
  )
  
  
  
}

# Custom JavaScript code for dynamically changing button color
jsCode <- "
  Shiny.addCustomMessageHandler('setButtonColor', function(message) {
    $('#loadBtn').css('background-color', message.color); // Set background color
    $('#loadBtn').css('color', 'white');                 // Ensure text is white
  });
"

shinyApp(
  ui = tagList(
    tags$head(tags$script(HTML(jsCode))),  # Include the JavaScript code
    ui
  ),
  server = server
)