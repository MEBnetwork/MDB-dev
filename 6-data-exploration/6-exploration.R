#### >>> 0. Start ####
#### >> 0.1 Load packages ####
library(shiny)
library(shinydashboard)
library(fresh)
library(shinyWidgets)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(DBI)
library(rmarkdown)
library(knitr)

#### >> 0.2 Theme for dashboard ####
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#89BE48"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#0F4476",
    dark_hover_bg = "#78b8c6",
    dark_color = "#000000"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#000000", 
    info_box_bg = "#000000"
  )
)

listlicence = c("CC-Zero", "CC-BY", "CC-BY-SA", "CC-BY-ND", "CC-BY-NC", "CC-BY-NC-SA", "CC-BY-NC-ND", "C")

#### >> 0.3 Database connection ####
# connection to the database 
# to change
# connexion <- dbConnect(RPostgres::Postgres(), 
#                        dbname = 'soiltemp', 
#                        host='localhost',
#                        port='5432',
#                        user='postgres',
#                        password='postgres')
connexion = connec
# loading the tables needed one by one
experiment <- dplyr::tbl(connexion, "experiments")

location <- dplyr::tbl(connexion, "locations")

loggers <- dplyr::tbl(connexion, "loggers")

timeseries <- dplyr::tbl(connexion, "meta_ts")

sites <- dplyr::tbl(connexion, "sites")

vegetation <- dplyr::tbl(connexion, "meta_veg_surveys")

# 
# habitat <- dplyr::tbl(connexion,  in_schema("extra", "habitat"))
# 
# subhabitat <- dplyr::tbl(connexion,  in_schema("extra", "subhabitat"))
# 
# world <- dplyr::tbl(connexion,  in_schema("extra", "world"))
# 
# # inner join to have the location in sites
# sites_location = inner_join(sites, location, by = c("location_id" = "id"))

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#### >>> 1. Server ####
ui <- dashboardPage(
  dashboardHeader(
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 75px}"),
            tags$style(".main-header .logo {height: 75px}")
    ),
    title = tags$img(src='https://www.soiltempproject.com/wp-content/uploads/2022/05/soiltemp.png', height=70)
  ),
  # style of sliders and recap
  dashboardSidebar(
    tags$head(tags$style(HTML('
    /* change sliders colors */
    .irs--shiny .irs-bar, .irs--shiny .irs-single {
    border: #f0ad4e;
    background: #f0ad4e;
    }
    
    /* for the recap at the bottom */
    .recap {
      text-align: center;
      font-size: 14px;
    }
    .sidebar {margin-top: 25px}'))),
    #### >> 1.1 Sidebar ####
    sidebarMenu(
      #### > 1.1.1 Location ####
      menuItem("Location",
               awesomeRadio("choiceconcoor",
                            "Continent/Country or coordinates :",
                            choices = list("Continent/Country" = 1, "Coordinates" = 2),
                            status = "warning"),
               uiOutput("choiceconcoord"),
               uiOutput("paysInput"),
               br()
      ),
      #### > 1.1.2 Experiments ####
      menuItem("Experiments",
               pickerInput(inputId = "expname", 
                           label = "Name :",
                           choices = sort(unique(experiment %>% pull(exp_name))),
                           selected = sort(unique(experiment %>% pull(exp_name))),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               awesomeCheckboxGroup(inputId = "manip", 
                                    label = "Manip :", 
                                    choices = list("Yes" = "true", "No" = "false"),
                                    selected = c("true", "false"),
                                    status = "warning"),
               awesomeCheckboxGroup(inputId = "insitu",
                                    label = "In situ :",
                                    choices = list("Yes" = "true", "No" = "false"),
                                    selected = c("true", "false"),
                                    status = "warning"),
               awesomeCheckboxGroup(inputId = "varmanip",
                                    label = "Climate variable manipulated :",
                                    choices = list("Yes" = "true", "No" = "false"),
                                    selected = c("true", "false"),
                                    status = "warning"),
               awesomeCheckboxGroup(inputId = "citizens",
                                    label = "Citizens science sampling :",
                                    choices = list("Yes" = "true", "No" = "false"),
                                    selected = c("true", "false"),
                                    status = "warning"),
               br()
      ),
      #### > 1.1.3 Sites ####
      menuItem("Sites",
               pickerInput(inputId = "sitename", 
                           label = "Name :",
                           choices = sort(unique(sites%>%pull(site_owner_id))),
                           selected = sort(unique(sites%>%pull(site_owner_id))),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               pickerInput(inputId = "habitatname", label = "Habitat :",
                           choices = sort(sites%>%pull(site_habitat)),
                           selected = sort(sites%>%pull(site_subhabitat)),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               uiOutput("opsubhabitat"),
               br()
      ),
      #### > 1.1.4 Loggers/Sensor ####
      menuItem("Loggers",
               pickerInput(inputId = "brandlog", 
                           label = "Logger's brand :",
                           choices = sort(unique(loggers%>%pull(log_brand))),
                           selected = sort(unique(loggers%>%pull(log_brand))),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               pickerInput(inputId = "typelog", 
                           label = "Logger's type :",
                           choices = sort(unique(loggers%>%pull(log_type))),
                           selected = sort(unique(loggers%>%pull(log_type))),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               awesomeCheckboxGroup(inputId = "agelog",
                                    label = "Logger's age:", 
                                    choices = list("Less than 1 year",
                                                   "1-5 years",
                                                   "5-10 years",
                                                   "More than 10 years"),
                                    selected = c("Less than 1 year",
                                                 "1-5 years",
                                                 "5-10 years",
                                                 "More than 10 years"),
                                    status = "warning"),
               awesomeCheckboxGroup(inputId = "shield",
                                    label = "Sensor covered by a shield ?",
                                    choices = list("Yes"="true","No"="false"),
                                    selected = c("true","false"),
                                    status = "warning"),
               uiOutput("ophomeshield"),
               br()
      ),
      #### > 1.1.5 Timeseries ####
      menuItem("Timeseries",
               pickerInput(inputId = "climvar",
                           label = "Climatic variable :",
                           choices = sort(unique(timeseries%>%pull(mts_clim_variable))),
                           selected = sort(unique(timeseries%>%pull(mts_clim_variable))),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               sliderInput(inputId = "climacc",
                           label = "Accuracy of the measurements :",
                           min = min(timeseries%>%pull(mts_clim_accuracy) |> as.numeric(), na.rm = TRUE)*100, max = max(timeseries%>%pull(mts_clim_accuracy) |> as.numeric(), na.rm = TRUE)*100, 
                           value = c(min(timeseries%>%pull(mts_clim_accuracy) |> as.numeric(), na.rm = TRUE)*100,max(timeseries%>%pull(mts_clim_accuracy) |> as.numeric(), na.rm = TRUE)*100)),
               sliderInput(inputId = "tempres",
                           label = "Temporal res",
                           min = min(timeseries%>%pull(mts_clim_temporal_res) |> as.numeric(), na.rm = TRUE),
                           max = max(timeseries%>%pull(mts_clim_temporal_res) |> as.numeric(), na.rm = TRUE),
                           value = c(min(timeseries%>%pull(mts_clim_temporal_res) |> as.numeric(), na.rm = TRUE), max(timeseries%>%pull(mts_clim_temporal_res) |> as.numeric(), na.rm = TRUE))),
               sliderInput(inputId = "sensheight",
                           label = "Height of measurements (cm) :",
                           min = min(timeseries%>%pull(mts_sensor_height) |> as.numeric(), na.rm = TRUE), max = max(timeseries%>%pull(mts_sensor_height) |> as.numeric(), na.rm = TRUE), 
                           value = c(min(timeseries%>%pull(mts_sensor_height) |> as.numeric(), na.rm = TRUE),max(timeseries%>%pull(mts_sensor_height) |> as.numeric(), na.rm = TRUE))),
               sliderInput(inputId = "sensrange",
                           label = "Range of height measured (cm) :",
                           min = min(timeseries%>%pull(mts_sensor_height_range) |> as.numeric(), na.rm = TRUE), max = max(timeseries%>%pull(mts_sensor_height_range) |> as.numeric(), na.rm = TRUE), 
                           value = c(min(timeseries%>%pull(mts_sensor_height_range) |> as.numeric(), na.rm = TRUE),max(timeseries%>%pull(mts_sensor_height_range) |> as.numeric(), na.rm = TRUE))),
               dateRangeInput(inputId = "daterangets",
                              label = "Date range input: yyyy-mm-dd",
                              start = min(timeseries%>%pull(mts_date_start), na.rm = TRUE), 
                              end = max(timeseries%>%pull(mts_date_stop), na.rm = TRUE)),
               pickerInput(inputId = "licts", 
                           label = "Licence :",
                           choices = sort(listlicence),
                           selected = sort(listlicence),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               br()
      ),
      #### > 1.1.6 Vegetation survey ####
      menuItem("Vegetation survey",
               pickerInput(inputId = "method",
                           label = "Sampling method :",
                           choices = sort(unique(vegetation%>%pull(mvs_method_short))),
                           selected = sort(unique(vegetation%>%pull(mvs_method_short))),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               awesomeCheckboxGroup("multilayers", "Several canopy layers :",
                                    choices = list("Yes"="true","No"="false"),
                                    selected = c("true","false"),
                                    status = "warning"),
               sliderInput("plotsize",
                           "Size of the plot :",
                           min = min(vegetation%>%pull(mvs_plot_size)),
                           max = max(vegetation%>%pull(mvs_plot_size)),
                           value = c(min(vegetation%>%pull(mvs_plot_size)),max(vegetation%>%pull(mvs_plot_size)))),
               dateRangeInput('daterangeveg',
                              label = 'Date range input: yyyy-mm-dd',
                              start = min(vegetation%>%pull(mvs_date), na.rm = TRUE), 
                              end = max(vegetation%>%pull(mvs_date), na.rm = TRUE)),
               
               
               pickerInput("licveg", 
                           "Licence :", 
                           choices = sort(listlicence), 
                           selected = sort(listlicence),
                           options = list(`actions-box` = TRUE,
                                          dropup = FALSE,
                                          width = "265px",
                                          `live-search` = TRUE),
                           multiple = TRUE),
               br()
      )
    )
  ), 
  #### >> 1.2 Body ####
  dashboardBody(
    use_theme(mytheme),
    #### > 1.2.1 Map ####
    fluidRow(
      leafletOutput("mymap")
    ),
    # #### > 1.2.2 Vizualisation ####
    fluidRow(column(width= 6,
                    uiOutput("plotnbobs_ui"))
    ),
    #### > 1.2.3 Summary ####
    fluidRow(
      htmlOutput("summary")
    ),
    #### > 1.2.4 Buttons for report ####
    fluidRow(
      downloadButton("summaryreport", "Summary"),
      actionButton("submissionreport", "Ask for data")
    )
  )
)


#### >>> 2. Server ####
server <- function(input,output) {
  #### >> 2.1 Location ####
  # if choice = continent, the pickerinput for continent appears
  output$choiceconcoord <- renderUI({
    if (input$choiceconcoor== "1") {
      cont <- list(
        sliderInput("alt", "Altitude :",
                    min = min(location%>%pull(loc_alt), na.rm = TRUE), max = max(location%>%pull(loc_alt), na.rm = TRUE), value = c(min(location%>%pull(loc_alt), na.rm = TRUE), max(location%>%pull(loc_alt), na.rm = TRUE))),
        pickerInput("locationglob",
                    "Continent :", 
                    choices = sort(unique(world$continent)), 
                    options = list(`actions-box` = TRUE,
                                   dropup = FALSE,
                                   width = "265px"),
                    multiple = TRUE,
                    selected = sort(unique(world$continent)))
      )
      tagList(cont)
    } else {
      # else, the cursors for lat and long appears
      coord <- list(
        sliderInput("lat", "Latitude :",
                    min = -180, max = 180, value = c(5, 15)),
        sliderInput("long", "Longitude :",
                    min = -90, max = 90, value = c(5, 15)),
        sliderInput("alt", "Altitude :",
                    min = min(location%>%pull(loc_alt), na.rm = TRUE), max = max(location%>%pull(loc_alt), na.rm = TRUE), value = c(min(location%>%pull(loc_alt), na.rm = TRUE), max(location%>%pull(loc_alt), na.rm = TRUE)))
      )
      tagList(coord)
    }
  })
  
  # add the selection of country
  output$paysInput <- renderUI({
    if (input$choiceconcoor== "1") { 
      pickerInput("country",
                  "Country :", 
                  choices = world_filtered()%>%select(name),
                  options = list(`actions-box` = TRUE,
                                 dropup = FALSE,
                                 width = "265px",
                                 `live-search` = TRUE),
                  multiple = TRUE,
                  selected = world_filtered()%>%pull(name))
    }
  })
  
  
  # add the is the shield homemade if there have the true at question shield selected
  output$ophomehield <- renderUI({
    if (all(input$shield == c("true","false")) | all(input$shield == c("true", "true"))) {
      awesomeCheckboxGroup( "homeshield",
                            "Is the shield homemade?",
                            choices = list("Yes" = "true", "No" = "false"),
                            selected = c("true","false"),
                            status = "warning")
    }
  })
  
  output$opsubhabitat <- renderUI({
    pickerInput("subhabitat",
                "Subhabitat :", 
                choices = subhabitat_filtered()|>select(name_subhabitat),
                options = list(`actions-box` = TRUE,
                               dropup = FALSE,
                               width = "265px",
                               `live-search` = TRUE),
                multiple = TRUE,
                selected = subhabitat_filtered()%>%select(name_subhabitat))
  })
  
  
  
  #### >> 2.2 Filters ####
  #### > 2.2.1 Location ####
  world_filtered <- reactive({
    world_filtered <- world %>% 
      filter(continent %in% input$locationglob)
    return(world_filtered)
  })
  
  country_filtered <-reactive({
    country_filtered <- world_filtered()%>%
      filter(name %in% input$country)
    return(country_filtered)
  })
  
  
  location_filtered <- reactive ({
    if (input$choiceconcoor== "1") {
    country_filtered = country_filtered()%>%select(country_code)
    alt1 = input$alt[1]
    alt2 = input$alt[2]
    location_filtered <- location %>%
      filter(loc_country %in% country_filtered &
               (between(loc_alt, alt1, alt2) | is.na(loc_alt)))
    return(location_filtered)
    }
    else {
      long1 = input$long[1]
      long2 = input$long[2]
      lat1 = input$lat[1]
      lat2 = input$lat[2]
      alt1 = input$alt[1]
      alt2 = input$alt[2]
      location_filtered <- location %>%
        filter((between(loc_long, long1, long2) | is.na(loc_long)) &
                 (between(loc_lat, lat1, lat2) | is.na(loc_lat)) &
                 (between(loc_alt, alt1, alt2) | is.na(loc_alt)))
    }
  })
  
  #### > 2.2.2 Experiments ####
  experiments_filtered <- reactive({
    experiments_filtered <- experiment %>%
      filter((exp_name %in% input$expname | is.na(exp_name)) &
               (exp_manip %in% input$manip | is.na(exp_manip)) & 
               (exp_insitu %in% input$insitu | is.na(exp_insitu)) & 
               (exp_clim_manip %in% input$varmanip | is.na(exp_clim_manip)) & 
               (exp_citizen %in% input$citizens | is.na(exp_citizen)))
    return(experiments_filtered)
  })
  
  #### > 2.2.3 Sites ####
  habitat_filtered <- reactive ({
    habitat_filtered <- habitat %>%
      filter(name %in% input$habitatname)
    return(habitat_filtered)
  })
  
  subhabitat_filtered <- reactive({
    id_hab = habitat_filtered()%>%select(id)
    subhabitat_filtered <- subhabitat %>%
      filter(id_habitat %in% id_hab)
    return(subhabitat_filtered)
  })
  
  sites_filtered <- reactive({
    loc_long_filt = location_filtered()%>%pull(loc_long)
    loc_lat_filt = location_filtered()%>%pull(loc_lat)
    loc_alt_filt = location_filtered()%>%pull(loc_alt)
    expe_filt = experiments_filtered()%>%pull(id)
    habitat_filt = habitat_filtered()%>%pull(id)
    sites_filtered <- sites_location %>%
      filter((loc_long %in% loc_long_filt | is.na(loc_long)) & 
               (loc_lat %in% loc_lat_filt | is.na(loc_lat)) &
               (loc_alt %in% loc_alt_filt | is.na(loc_alt)) &
               (experiment_id %in% expe_filt | is.na(experiment_id)) & 
               (sit_owner_id %in% input$sitename | is.na(sit_owner_id)) &
               (sit_habitat %in% habitat_filt| is.na(sit_habitat)))
    return(sites_filtered)
  })
  
  #### > 2.2.4 Loggers ####
  loggers_filtered <- reactive ({
    loggers_filtered <- loggers %>%
      filter((log_brand %in% input$brandlog | is.na(log_brand)) &
               (log_type %in% input$typelog | is.na(log_type)) &
               (log_age %in% input$agelog | is.na(log_age)))
    return(loggers_filtered)
  })
  
  #### > 2.2.5 Timeseries ####
  timeseries_filtered <- reactive ({
    id_site = sites_filtered()%>% pull(id)
    id_log = loggers_filtered()%>% pull(id)
    acc1 = input$climacc[1]/100
    acc2 = input$climacc[2]/100
    tempres1 = input$tempres[1]
    tempres2 = input$tempres[2]
    height1 = input$sensheight[1]
    height2 = input$sensheight[2]
    heightrange1 = input$sensrange[1]
    heightrange2 = input$sensrange[2]
    date1 = input$daterangets[1]
    date2 = input$daterangets[2]
    timeseries_filtered <- timeseries %>%
      filter((site_id %in% id_site | is.na(site_id)) &
               (logger_id %in% id_log | is.na(logger_id)) &
               (mts_sensor_shielding %in% input$shield | is.na(mts_sensor_shielding)) &
               (mts_homemade_shield %in% input$homeshield | is.na(mts_homemade_shield)) &
               (mts_clim_variable %in% input$climvar | is.na(mts_clim_variable)) &
               (between(mts_clim_accuracy, acc1, acc2) | is.na(mts_clim_accuracy)) &
               (between(mts_clim_temporal_res,tempres1, tempres2) | is.na(mts_clim_temporal_res)) &
               (between(mts_sensor_height, height1, height2) | is.na(mts_sensor_height)) &
               # (between(mts_sensor_height_range, heightrange1, heightrange2) | is.na(mts_sensor_height_range)) &
               (between(mts_date_start, date1, date2) | is.na(mts_date_start)) &
               (between(mts_date_stop, date1, date2) | is.na(mts_date_stop)) &
               (mts_licence %in% input$licts | is.na(mts_licence)))
    return(timeseries_filtered)
  })
  
  #### > 2.2.6 Vegetation ####
  vegetation_filtered <- reactive ({
    id_site = sites_filtered()%>% pull(id)
    date1 = input$daterangeveg[1]
    date2 = input$daterangeveg[2]
    plotsize1 = input$plotsize[1]
    plotsize2 = input$plotsize[2]
    vegetation_filtered <- vegetation #%>%
      # filter((site_id %in% id_site | is.na(site_id)) &
      #          (between(mvs_date, date1, date2) | is.na(mvs_date)) &
      #          (mvs_method_short %in% input$method | is.na(mvs_method_short)) &
      #          (mvs_multilayer_vegetation %in% input$multilayers | is.na(mvs_multilayer_vegetation)) &
      #          (mvs_licence %in% input$licveg | is.na(mvs_licence)) &
      #          (between(mvs_plot_size, plotsize1, plotsize2) | is.na(mvs_plot_size)))
    return(vegetation_filtered)
  })
  
  #### >> 2.3 Map ####
  output$mymap <- renderLeaflet({
    # Creation of the map
    map <- leaflet() %>%
      addTiles() %>%
      # Add bounds
      fitBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      # Set view at the ~ center of the map
      setView(lng = 0, lat = 25, zoom = 2)
    
    # Add markers
    map <- map %>% addMarkers(
      # Taking longitude and latitude from sites_filtered()
      lng = sites_filtered() %>% pull(loc_long) |> round(digits = 3),
      lat = sites_filtered() %>% pull(loc_lat) |> round(digits = 3),
      # Adding clusters
      clusterOptions = markerClusterOptions(),
      # Adding pop-up
      popup = paste("Site's Name :", sites_filtered() %>% pull(sit_owner_id)))
    
    # Recentering
    map <- map %>% htmlwidgets::onRender("
    function(el, x) {
      this.on('drag', function(e) {
        this.panInsideBounds([[90, -180], [-90, 180]], { animate: false });
      });
    }")
  })
  
  
  
  
  #### >> 2.4 Vizualisation ####
  # timeseries_dates_filtered : dates with the number of obs 
  timeseries_dates_filtered <- reactive({
    # count the total number of timeseries measures
    total_clim_measures <- timeseries_filtered() %>% 
      summarize(count = n()) %>% pull(count)
    
    if (total_clim_measures == 0) {
      return(NULL)
    }
    
    # create a sequence of dates for the whole observation period
    all_dates <- seq(min(timeseries_filtered() %>% pull(mts_date_start)), 
                     max(timeseries_filtered() %>% pull(mts_date_stop)), 
                     by = "month")
    
    # count the number of observation for each dates
    dates <- map_df(.x = all_dates,
                    .f = ~{
                      n <- timeseries_filtered() %>%
                        filter(mts_date_start <= .x & mts_date_stop >= .x) %>% 
                        collect() %>%
                        nrow()
                      data.frame(date = .x, n = n)
                    })
    return(dates)
  })
  
  # vegetation_obs_filtered : dates with the number of obs
  vegetation_obs_filtered <- reactive({
    # count the total number of vegetation surveys
    total_veg_survey <- vegetation_filtered() %>% 
      summarize(count = n()) %>% 
      pull(count)
    
    if (total_veg_survey == 0) {
      return(NULL)
    }
    
    # count the number of vegetation surveys for each dates
    observation_veg <- aggregate(id ~ mvs_date, vegetation_filtered(), length)
    colnames(observation_veg) <- c("date", "count")
    return(observation_veg)
  })
  
  scale_factor <- reactive({
    timeseries_data <- timeseries_dates_filtered()
    vegetation_data <- vegetation_obs_filtered()
    
    if (is.null(timeseries_data) || is.null(vegetation_data)) {
      return(NULL)
    }
    
    nbmaxts <- max(timeseries_data %>% pull(n))
    nbmaxveg <- max(vegetation_data %>% pull(count))
    
    # create a scale factor to adjust the bars relative to the line
    return(nbmaxts / nbmaxveg)
  })
  
  # Create UI output for plot or error message
  output$plotnbobs_ui <- renderUI({
    timeseries_data <- timeseries_dates_filtered()
    vegetation_data <- vegetation_obs_filtered()
    
    if (is.null(timeseries_data) || is.null(vegetation_data)) {
      return(div(style = "color: red; font-weight: bold;", "The graph cannot be created, missing vegetation or timeseries data."))
    } else {
      plotOutput("plotnbobs")
    }
  })
  
  # plot creation
  output$plotnbobs <- renderPlot({
    timeseries_data <- timeseries_dates_filtered()
    vegetation_data <- vegetation_obs_filtered()
    scale_factor_value <- scale_factor()
    
    if (is.null(timeseries_data) || is.null(vegetation_data) || is.null(scale_factor_value)) {
      return(NULL)
    }
    
    # create the plot
    ggplot() +
      # adding a line for the timeseries
      geom_line(data = timeseries_data, aes(x = date, y = n), 
                color = "#CF0A0A") +
      # adding bars for the vegetation surveys
      geom_bar(data = vegetation_data, aes(x = date, y = count * scale_factor_value), 
               stat = "identity", fill = "#50AB42", alpha = 0.5) +
      # add the scale 
      scale_y_continuous(name = "Number of observations",
                         sec.axis = sec_axis(~ . / scale_factor_value, 
                                             name = "Number of vegetation survey")) +
      # add the labels
      labs(title = "Number of observations by time", x = "Date") +
      # change colors
      theme(axis.title.y.right = element_text(color = "#326B29"),
            axis.text.y.right = element_text(color = "#326B29"),
            axis.title.y.left = element_text(color = "#6B0505"),
            axis.text.y.left = element_text(color = "#6B0505"))
  })
  
  
  
  
  
  #### >> 2.5 Summary ####
  output$summary <- renderUI({
    total_sit <- sites_filtered() %>% summarize(count = n()) %>% pull(count)
    total_exp <- experiments_filtered() %>% summarize(count = n()) %>% pull(count)
    total_log <- loggers_filtered() %>% summarize(count = n()) %>% pull(count)
    total_clim_measures <- timeseries_filtered() %>% summarize(count = n()) %>% pull(count)
    total_veg_survey <- vegetation_filtered() %>% summarize(count = n()) %>% pull(count)
    nbexperiments = nrow(experiments_filtered())
    nbloggers = loggers_filtered() %>% count(id)
    nbclimmeasure = timeseries_filtered() %>% count(id)
    nbvegsurvey = vegetation_filtered() %>% count(id)
    
    HTML(paste0("<div class = 'recap' >
    Number of differents sites :", total_sit, "<br>
    Number of differents experiments :", total_exp, "<br>
    Number of differents loggers :", total_log, "<br>
    Number of differents climatic measures :", total_clim_measures,"<br>
    Number of differents vegetation survey :", total_veg_survey, "<br>
         </div>"))
  }) 
  
  
  
  
  #### >> 2.6 SQL Queries ####
  query_loc <- reactive({
    query_loc <- sql_render(location_filtered())
    return(query_loc)
  })
  
  query_exp <- reactive({
    query_exp <- sql_render(experiments_filtered())
    return(query_exp)
  })
  
  query_sit <- reactive({
    query_sit <- sql_render(sites_filtered())
    return(query_sit)
  })
  
  query_log <- reactive({
    query_log <- sql_render(loggers_filtered())
    return(query_log)
  })
  
  query_ts <- reactive({
    query_ts <- sql_render(timeseries_filtered())
    return(query_ts)
  })
  
  query_veg <- reactive({
    query_veg <- sql_render(vegetation_filtered())
    return(query_veg)
  })
  
  
  #### >> 2.7 Reports ####
  user_info <- reactiveValues(name = NULL, surname = NULL, email = NULL, projectname = NULL, projectsummary = NULL)

  # pop up for entering the users infos
  observeEvent(input$submissionreport, {
    showModal(modalDialog(
      title = "Please fill in your information",
      tagList(
        textInput("name", "Last name:"),
        textInput("surname", "Name:"),
        textInput("email", "Email:"),
        textInput("projectname", "Name of your project:"),
        textInput("summaryproject", "Summary of your project:"),
        actionButton("cancel_button", "Cancel"),
        actionButton("confirm_button", "OK")
      ),
      easyClose = FALSE,
      footer = NULL
    ))
  })
  
  
  observeEvent(input$confirm_button, {
    user_info$name <- input$name
    user_info$surname <- input$surname
    user_info$email <- input$email
    user_info$projectname <- input$projectname
    user_info$projectsummary <- input$summaryproject
    
    removeModal()
    
    # download pop up
    showModal(modalDialog(
      title = "Submission report",
      downloadButton("submissionreport", "Download Now"),
      easyClose = TRUE
    ))
  })
  
  
  #### > 2.7.1 Summary Report ###
  output$summaryreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "summary_report.pdf",
    content = function(file) {
      # Check if the graph is created successfully
      if (is.null(timeseries_dates_filtered()) || is.null(vegetation_obs_filtered()) || is.null(scale_factor())) {
        showNotification("Cannot generate report: Missing data for visualization", type = "error")
        return(NULL)
      }
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "summary_report.Rmd")
      file.copy("summary_report.Rmd",
                tempReport,
                overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        #location
        choiceconcoor = input$choiceconcoor,
        continent = input$locationglob,
        country = input$country,
        alt = input$alt,
        lat = input$lat,
        long = input$long,
        #experiments
        expname = input$expname,
        manip = input$manip,
        insitu = input$insitu,
        climvarmanip = input$varmanip,
        citizens = input$citizens,
        #sites
        sitname = input$sitename,
        habitat = input$habitat,
        #loggers
        logbrand = input$brandlog,
        logtype = input$typelog,
        logage = input$agelog,
        logshield = input$shield,
        loghomeshield = input$homeshield,
        #timeseries
        climvar = input$climvar,
        accuracy = input$climacc,
        tempres = input$tempres,
        sensheight = input$sensheight,
        sensrange = input$sensrange,
        daterangets = input$daterangets,
        licts = input$licts,
        #vegetation
        sampmeth = input$method,
        multilayers = input$multilayers,
        plotsize = input$plotsize,
        daterangeveg = input$daterangeveg,
        licveg = input$licveg,
        #sqlqueries
        queryloc = query_loc(),
        queryexp = query_exp(),
        querysit = query_sit(),
        querylog = query_log(),
        queryts = query_ts(),
        queryveg = query_veg(),
        #table 
        tableloc = location_filtered() %>% collect(),
        tableexp = experiments_filtered() %>% collect(),
        tablesit = sites_filtered() %>% collect(),
        tablelog = loggers_filtered() %>% collect(),
        tablets = timeseries_filtered() ,
        tableveg = vegetation_filtered() %>% collect()
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = params,
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #### 2.7.2 Submission Report ####
  output$submissionreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "submission_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "submission_report.Rmd")
      file.copy("submission_report.Rmd",
                tempReport,
                overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(#people
        peoplename = user_info$name,
        surname = user_info$surname,
        email = user_info$email,
        projectname = user_info$projectname,
        projectsummary = user_info$projectsummary,
        #location
        choiceconcoor = input$choiceconcoor,
        continent = input$locationglob,
        country = input$country,
        alt = input$alt,
        lat = input$lat,
        long = input$long,
        #experiments
        expname = input$expname,
        manip = input$manip,
        insitu = input$insitu,
        climvarmanip = input$varmanip,
        citizens = input$citizens,
        #sites
        sitname = input$sitename,
        habitat = input$habitatname,
        #loggers
        logbrand = input$brandlog,
        logtype = input$typelog,
        logage = input$agelog,
        logshield = input$shield,
        loghomeshield = input$homeshield,
        #timeseries
        climvar = input$climvar,
        accuracy = input$climacc,
        tempres = input$tempres,
        sensheight = input$sensheight,
        sensrange = input$sensrange,
        daterangets = input$daterangets,
        licts = input$licts,
        #vegetation
        sampmeth = input$method,
        multilayers = input$multilayers,
        plotsize = input$plotsize,
        daterangeveg = input$daterangeveg,
        licveg = input$licveg,
        #sqlqueries
        queryloc = query_loc(),
        queryexp = query_exp(),
        querysit = query_sit(),
        querylog = query_log(),
        queryts = query_ts(),
        queryveg = query_veg(),
        #table 
        tableloc = location_filtered() %>% collect(),
        tableexp = experiments_filtered() %>% collect(),
        tablesit = sites_filtered() %>% collect(),
        tablelog = loggers_filtered() %>% collect(),
        tablets = timeseries_filtered() %>% collect(),
        tableveg = vegetation_filtered() %>% collect()
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = params,
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

#### >>> 3. Launch ####
shinyApp(ui, server)