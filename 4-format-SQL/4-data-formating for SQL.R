# Load required libraries
#rm(list =ls())
library(tidyverse)
library(trustedtimestamping)
library(openssl)
library(lubridate)
# Encryption key
passphrase <- charToRaw("MDB secret code phrase")
key <- sha256(passphrase)

#### Loading the dataset ####
#data.path = "C:\\Bedassa\\SoilTemp_Processed\\MDB-dev\\0-data\\bexis_download\\"
# '0-data/bexis_download/'
listing.bexis = read.csv(paste0(data.path, 'listing-bexis.csv'))
list.files = list.files(data.path)
list.zip.files = list.files[grep('BEXIS.zip', list.files)]

i = 1
formate.dataset.to.sql = function(i, list.zip.files, listing.bexis, data.path){
  dataset.id = list.zip.files[i] |> 
    str_remove('dataset-') |> 
    str_remove('_BEXIS.zip') |> 
    as.numeric()
  
  dataset.doi = 
    listing.bexis$DOI[listing.bexis$dataset == dataset.id & grepl('BEXIS.zip', listing.bexis$attachment.name)]
  
  dataRDS = 
    paste0(data.path, list.zip.files[i]) |> 
    unzip() |>
    read_rds() |> 
    aes_cbc_decrypt(key = key) |>
    unserialize()
  
  #### > 2. Prepare datasets for push ####
  #### >> 2.1. People ####
  people_data <- data.frame(
    ppl_firstname = dataRDS$DATA$PEOPLE$FIRSTNAME |> as.character(),
    ppl_middlename_initials = dataRDS$DATA$PEOPLE$MIDDLENAME_INITIALS|> as.character(),
    ppl_lastname = dataRDS$DATA$PEOPLE$LASTNAME |> as.character(),
    ppl_coauthor_name = dataRDS$DATA$PEOPLE$COAUTHOR_NAME |> as.character(),
    ppl_email = dataRDS$DATA$PEOPLE$EMAIL |> as.character(),
    ppl_second_email = dataRDS$DATA$PEOPLE$SECOND_EMAIL |> as.character(),
    ppl_orcid = dataRDS$DATA$PEOPLE$ORCID |> as.character(),
    ppl_bexis_id = dataset.id |> as.integer(),
    ppl_soiltemp_update = dataRDS$DATA$PEOPLE$SOILTEMP_UPDATE |> as.logical(),
    ppl_update = 'NO'|> as.character()
  )
  
  #### >> 2.2. Locations ####
  locations_data <- data.frame(
    loc_long = dataRDS$DATA$METADATA$LONGITUDE |>as.numeric(), #TODO to be rounded 
    loc_lat = dataRDS$DATA$METADATA$LATITUDE |>as.numeric(), #TODO to be rounded 
    loc_alt = dataRDS$DATA$METADATA$ELEVATION |>as.numeric(),
    loc_country = dataRDS$DATA$METADATA$COUNTRY_CODE |> as.character(),
    loc_update = 'NO' |> as.character()
  ) |> 
    distinct()
  
  #### >> 2.3. Experiments ####
  experiments_data <- data.frame(
    exp_name = dataRDS$DATA$METADATA$EXPERIMENT_NAME |> as.character(),
    exp_manip = dataRDS$DATA$METADATA$EXPERIMENTAL_MANIPULATION |> as.logical(),
    exp_insitu = dataRDS$DATA$METADATA$EXPERIMENT_INSITU |> as.logical(),
    exp_clim_manip = dataRDS$DATA$METADATA$EXPERIMENT_CLIMATE |> as.logical(),
    exp_citizen = dataRDS$DATA$METADATA$EXPERIMENT_CITIZENS |> as.logical(),
    exp_design = dataRDS$DATA$METADATA$EXPERIMENT_DESIGN |> as.character(),
    exp_doi = dataRDS$DATA$METADATA$EXPERIMENT_DOI |> as.character(),
    exp_bexis_id = dataset.id |> as.integer(),
    exp_comment = dataRDS$DATA$METADATA$EXPERIMENT_COMMENT |> as.character(),
    exp_update = 'NO' |> as.character()
  ) |> 
    distinct()
  
  #### >> 2.4. Sites ####
  sites_data <- data.frame(
    exp_id = dataRDS$DATA$METADATA$EXPERIMENT_NAME |> as.character(), # to be updated in exp_id when pushing
    loc_long = dataRDS$DATA$METADATA$LONGITUDE |>as.numeric(), #TODO to be rounded 
    loc_lat = dataRDS$DATA$METADATA$LATITUDE |>as.numeric(), #TODO to be rounded 
    sif_id = 9999 |> as.integer(),
    site_owner_id = dataRDS$DATA$METADATA$SITE_ID |> as.character(),
    site_spatial_res = dataRDS$DATA$METADATA$GPS_ACCURACY |> as.integer(),
    site_long = dataRDS$DATA$METADATA$LONGITUDE |> as.numeric(),
    site_lat = dataRDS$DATA$METADATA$LATITUDE |> as.numeric(),
    site_alt = dataRDS$DATA$METADATA$ELEVATION |> as.numeric(),
    site_habitat = dataRDS$DATA$METADATA$HABITAT_TYPE |> as.integer(),
    site_subhabitat = dataRDS$DATA$METADATA$HABITAT_SUB_TYPE |> as.integer(),
    site_bexis_id = dataset.id |> as.integer(),
    site_comment = dataRDS$DATA$METADATA$SITE_COMMENTS |> as.character(),
    site_update = 'NO' |> as.character()
  ) |>
    distinct()
  
  #### >> 2.5. Loggers ####
  loggers_data <- data.frame(
    lgf_id = 9999 |> as.integer(), 
    log_owner_id = dataRDS$DATA$METADATA$LOGGER_CODE |> as.character(),
    log_serial_number = dataRDS$DATA$METADATA$LOGGER_SERIAL_NUMBER |> as.character(),
    log_brand = dataRDS$DATA$METADATA$LOGGER_BRAND |> as.character(),
    log_type = dataRDS$DATA$METADATA$LOGGER_TYPE |> as.character(),
    log_age = dataRDS$DATA$METADATA$LOGGER_AGE |> as.character(),
    log_bexis_id = dataset.id |> as.integer(),
    log_comment = dataRDS$DATA$METADATA$LOGGER_COMMENT |> as.character(),
    log_update = 'NO' |> as.character()
  ) |> 
    distinct()
  
  #### >> 2.6. Meta_ts ####
  
  meta_ts_sensor_height_range <- dataRDS$DATA$METADATA$SENSOR_LENGTH
  meta_ts_sensor_height_range <- gsub("[^0-9.]", "", meta_ts_sensor_height_range)  # Remove non-numeric characters
  meta_ts_sensor_height_range[meta_ts_sensor_height_range == ""] <- NA            # Replace empty strings with NA
  meta_ts_sensor_height_range <- as.numeric(meta_ts_sensor_height_range)     
  
  meta_ts_data <- data.frame(
  #  mts_id = dataRDS$DATA$METADATA$META_ID, #TODO update with the database meta_ts_id
    site_id =  dataRDS$DATA$METADATA$SITE_ID,
    log_id = dataRDS$DATA$METADATA$LOGGER_CODE, #TODO update with the database log_id
    mtf_id = 9999 |> as.integer(),
    mts_owner_id = dataRDS$DATA$METADATA$RAW_DATA_IDENTIFIER |> as.character(),
    mts_sensor_code  = dataRDS$DATA$METADATA$SENSOR_CODE |> as.character(),
    mts_sensor_shielding = dataRDS$DATA$METADATA$SENSOR_SHIELDING |> as.logical(),
    mts_homemade_shield = dataRDS$DATA$METADATA$SENSOR_SHIELDING_TYPE |> as.logical(),
    mts_clim_variable = dataRDS$DATA$METADATA$MICROCLIMATE_MEASUREMENT |> as.character(),
    mts_clim_unit = dataRDS$DATA$METADATA$UNIT |> as.character(),
    mts_clim_accuracy = dataRDS$DATA$METADATA$SENSOR_ACCURACY |> as.character(),
    mts_clim_temporal_res = dataRDS$DATA$METADATA$TEMPORAL_RESOLUTION |> as.character(),
    mts_sensor_height = dataRDS$DATA$METADATA$SENSOR_HEIGHT |> as.numeric(),
    mts_sensor_height_range =   meta_ts_sensor_height_range |> as.numeric(),
    mts_timezone_utc_offset = dataRDS$DATA$METADATA$TIME_DIFFERENCE |>as.numeric(),
    mts_date_start <- dataRDS$DATA$METADATA %>%
      mutate(mts_date_start = make_date(START_DATE_YEAR, START_DATE_MONTH, START_DATE_DAY))%>%
      dplyr:: select(mts_date_start),
    mts_date_stop<- dataRDS$DATA$METADATA %>%
      mutate(mts_date_stop = make_date(END_DATE_YEAR, END_DATE_MONTH, END_DATE_DAY))%>%
      dplyr::select(mts_date_stop),
    mts_licence = dataRDS$DATA$METADATA$LICENCE |> as.character(),
    mts_doi = dataset.doi |> as.character(),
    mts_bexis_id = dataset.id |> as.integer(),
    mts_ts_comment = dataRDS$DATA$METADATA$SENSOR_COMMENTS |> as.character(),
    mts_update = 'NO' |> as.character()
  )
  
  #### >> 2.7. Clim_ts ####
 
  # Extract relevant columns
  YEAR  <- as.integer(dataRDS$DATA$TIMESERIES$YEAR)
  MONTH <- as.integer(dataRDS$DATA$TIMESERIES$MONTH)
  DAY   <- as.integer(dataRDS$DATA$TIMESERIES$DAY)
  TIME  <- as.integer(dataRDS$DATA$TIMESERIES$TIME)
  
  # Build datetime strings for POSIXct
  datetime_str <- ifelse(
    !is.na(TIME),
    paste(YEAR, MONTH, DAY, TIME, "00", "00", sep = "-"),
    NA
  )
  
  # Convert to POSIXct
  cts_timestamp <- as.POSIXct(datetime_str, format = "%Y-%m-%d-%H-%M-%S", tz = "UTC")
  
  # Build data frame
  clim_ts_data <- data.frame(
    ctf_id = rep(9999L, length(cts_timestamp)),
    cts_timestamp = cts_timestamp,
    cts_value = as.numeric(dataRDS$DATA$TIMESERIES$CTS_VALUES),
    cts_sensor_id = as.character(dataRDS$DATA$TIMESERIES$RAW_DATA_IDENTIFIER),
    cts_update = rep("NO", length(cts_timestamp)),
    cts_bexis_id = dataset.id |> as.integer(),
    stringsAsFactors = FALSE
  )
  
  head(clim_ts_data)
  
  
  if(!is.na(dataRDS$DATA$VEGETATION.METADATA)){
    #### >> 2.8. Metadata vegetation survey ####
    if(nrow(dataRDS$DATA$VEGETATION.METADATA)>0){
      meta_vega_surveys_data <- data.frame(
        site_id = dataRDS$DATA$VEGETATION.METADATA$SITE_ID, #TODO to be updated vith database ID 
        mvs_date = dataRDS$DATA$VEGETATION.METADATA$DATE |> as.Date(),
        mvs_plot_size = dataRDS$DATA$VEGETATION.METADATA$PLOT_SIZE |> as.numeric(),
        mvs_method_short = dataRDS$DATA$VEGETATION.METADATA$SURVEY_METHOD_SHORT |>as.character(),
        mvs_method_long = dataRDS$DATA$VEGETATION.METADATA$SURVEY_METHOD_LONG |>as.character(),
        mvs_tax_ref = dataRDS$DATA$VEGETATION.METADATA$`TAXONOMIC REFERENCE` |> as.character(),
        mvs_multilayer_vegetation = dataRDS$DATA$VEGETATION.METADATA$`MULTILAYER VEGETATION` |>as.logical(),
        mvs_other = dataRDS$DATA$VEGETATION.METADATA$OTHER |> as.character(),
        mvs_unit = dataRDS$DATA$VEGETATION.METADATA$`OTHER UNIT` |> as.character(),
        mvs_doi = dataRDS$DATA$VEGETATION.METADATA$DOI |>as.character(),
        mvs_licence = dataRDS$DATA$VEGETATION.METADATA$LICENCE |> as.character(),
        mvs_bexis_id =  dataset.id |> as.integer(),
        mvs_comment = dataRDS$DATA$VEGETATION.METADATA$COMMENT |> as.character(),
        mvs_update = 'NO' |> as.character()
      )
    }else{
      meta_vega_surveys_data = NA
    }
    
    #### >> 2.9. vegetation survey traits ####
    if(nrow(dataRDS$DATA$VEGETATION.METADATA)>0){
      veg_surveys_traits_data <- 
        data.frame(
          vsf_id = 9999 |> as.integer(), 
          mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
          vst_trait = 'TOTAL BIOMASS' |> as.character(),
          vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`TOTAL BIOMASS UNIT` ,
          vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`TOTAL BIOMASS` |> as.numeric(),
          vst_trait_bexis_id = dataset.id |> as.integer(),
          vst_update = 'NO' |> as.character()
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'TOTAL PLANT COVER' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`TOTAL PLANT COVER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |>as.integer(), #TODO update upon submission
            vst_trait = 'COVER E3: TREE LAYER' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`COVER E3: TREE LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'COVER E2: SHRUB LAYER' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`COVER E2: SHRUB LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'COVER E1: HERB LAYER' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`COVER E1: HERB LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |>as.integer(), #TODO update upon submission
            vst_trait = 'COVER E0: MOSS LAYER' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`COVER E0: MOSS LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |>as.integer(), #TODO update upon submission
            vst_trait = 'COVER ROCK' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`COVER E1: HERB LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 , 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'COVER BARESOIL' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`COVER UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`COVER E1: HERB LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |>as.integer(), #TODO update upon submission
            vst_trait = 'LAI' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`LAI UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`LAI`|> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |>as.integer(), #TODO update upon submission
            vst_trait = 'SPECIES RICHNESS' |> as.character(),
            vst_trait_unit = "# of species" |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`SPECIES RICHNESS` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'HEIGHT E3: TREE LAYER' |> as.character(),
            vst_trait_unit = "m" |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`HEIGHT E3: TREE LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |>as.integer(), #TODO update upon submission
            vst_trait = 'HEIGHT E2: SHRUB LAYER' |> as.character(),
            vst_trait_unit = "m" |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`HEIGHT E2: SHRUB LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'HEIGHT E1: HERB LAYER' |> as.character(),
            vst_trait_unit = "m" |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`HEIGHT E1: HERB LAYER` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character() 
          )
        ) |> 
        bind_rows(
          data.frame(
            vsf_id = 9999 |> as.integer(), 
            mvs_id = dataRDS$DATA$VEGETATION.METADATA$SURVEY_ID |> as.integer(), #TODO update upon submission
            vst_trait = 'SOIL BULK DENSITY' |> as.character(),
            vst_trait_unit = dataRDS$DATA$VEGETATION.METADATA$`SOIL BULK DENSITY UNIT` |> as.character(),
            vst_trait_value = dataRDS$DATA$VEGETATION.METADATA$`SOIL BULK DENSITY` |> as.numeric(),
            vst_trait_bexis_id = dataset.id |> as.integer(),
            vst_update = 'NO' |> as.character()
          )
        ) |> 
        filter(!is.na(vst_trait_value))
    }else{
      veg_surveys_traits_data = NA
    }
    
    #### >> 2.10. species ####
    if(nrow(dataRDS$DATA$VEGETATION.DATA)>0){
      species_data <- data.frame(
        # mvs_id = dataRDS$DATA$VEGETATION.DATA$SITE_ID |>as.integer(), #TODO to be updated with database ID 
        spc_name = dataRDS$DATA$VEGETATION.DATA$SPECIES_NAME |> as.character(),
        spc_bexis_id = dataset.id |> as.integer(),
        spc_comment = dataRDS$DATA$VEGETATION.DATA$COMMENTS |> as.character(),
        spc_update = "NO" |> as.character()
      )
    }else{
      species_data = NA
    }
    
    
    #### >> 2.11. Species traits #### 
    
    spt_trait <- dataRDS$DATA$SPECIES.DETAILS$SPECIES_TRAIT |> as.character()
    spt_trait_unit <- dataRDS$SPECIES.DETAILS$SPECIES_TRAIT_UNIT |> as.character()
    spt_trait_value <- gsub("[^0-9.]", "", dataRDS$DATA$SPECIES.DETAILS$SPECIES_TRAIT_VALUE)  # Remove non-numeric characters
    spt_trait_value[spt_trait_value == ""] <- NA  # Replace empty strings with NA
    spt_trait_value <- as.numeric(spt_trait_value)
    spt_trait_bexis_id <- dataset.id |> as.integer()
    
    # Handle NULL or zero-length vectors
    if (is.null(spt_trait) || length(spt_trait) == 0) spt_trait <- NA
    if (is.null(spt_trait_unit) || length(spt_trait_unit) == 0) spt_trait_unit <- NA
    if (is.null(spt_trait_value) || length(spt_trait_value) == 0) spt_trait_value <- NA
    if (is.null(spt_trait_bexis_id) || length(spt_trait_bexis_id) == 0) spt_trait_bexis_id <- NA
    
    # Determine the maximum length
    max_length <- max(
      length(spt_trait),
      length(spt_trait_unit),
      length(spt_trait_value),
      length(spt_trait_bexis_id)
    )
    
    # Pad shorter columns with NA
    spt_trait <- c(spt_trait, rep(NA, max_length - length(spt_trait)))
    spt_trait_unit <- c(spt_trait_unit, rep(NA, max_length - length(spt_trait_unit)))
    spt_trait_value <- c(spt_trait_value, rep(NA, max_length - length(spt_trait_value)))
    spt_trait_bexis_id <- c(spt_trait_bexis_id, rep(spt_trait_bexis_id, max_length - length(spt_trait_bexis_id)))
    
    
    if(nrow(dataRDS$DATA$SPECIES.DETAILS)>0){
      
      species_traits_data <- data.frame(
        # spc_id = spc_id |> as.integer() , #TODO to be updated using the site id and mvs_id
        spt_trait = spt_trait|> as.character(),
        spt_trait_unit = spt_trait_unit|> as.character(),
        spt_trait_value = spt_trait_value|> as.numeric(),
        spt_trait_bexis_id = spt_trait_bexis_id|> as.integer(),
        spt_update = rep('NO', max_length)|> as.character(), # Fill with 'NO' to match the length
        stringsAsFactors = FALSE
      )|> 
        rbind(
          species_traits_data <- data.frame(
            # spc_id = spc_id |> as.integer(), #TODO to be updated using the site id and mvs_id
            spt_trait = "PRESENCE" |> as.character(),
            spt_trait_unit = "T/F" |> as.logical(),
            spt_trait_value = dataRDS$DATA$VEGETATION.DATA$PRESENCE |>as.numeric(),
            spt_trait_bexis_id = dataset.id |> as.integer(),
            spt_update = 'NO' |> as.character()
          )
        ) |>
        rbind(
          species_traits_data <- data.frame(
            # spc_id = spc_id |> as.integer(), #TODO to be updated using the site id and mvs_id
            spt_trait = "BIOMASS" |> as.character(),
            spt_trait_unit = dataRDS$DATA$VEGETATION.DATA$BIOMASS_UNIT |> as.character(),
            spt_trait_value = dataRDS$DATA$VEGETATION.DATA$BIOMASS |> as.numeric(),
            spt_trait_bexis_id = dataset.id |> as.integer(),
            spt_update = 'NO' |> as.character()
          )
        ) |>
        rbind(
          species_traits_data <- data.frame(
            # spc_id = spc_id |> as.integer(), #TODO to be updated using the site id and mvs_id
            spt_trait = "COVER" |> as.character(),
            spt_trait_unit = dataRDS$DATA$VEGETATION.DATA$COVER_UNIT |> as.character(),
            spt_trait_value = dataRDS$DATA$VEGETATION.DATA$COVER |> as.numeric(),
            spt_trait_bexis_id = dataset.id |> as.integer(),
            spt_update = 'NO' |> as.character()
          )
        ) |>
        rbind(
          species_traits_data <- data.frame(
            #  spc_id = spc_id |> as.integer()  , #TODO to be updated using the site id and mvs_id
            spt_trait = "LAI" |> as.character(),
            spt_trait_unit = "%" |> as.character(),
            spt_trait_value = dataRDS$DATA$VEGETATION.DATA$LAI |> as.numeric(),
            spt_trait_bexis_id = dataset.id |> as.integer(),
            spt_update = 'NO' |> as.character()
          )
        ) |>
        rbind(
          species_traits_data <- data.frame(
            # spc_id = spc_id |> as.integer(), #TODO to be updated using the site id and mvs_id
            spt_trait = "DENSITY" |> as.character(),
            spt_trait_unit = dataRDS$DATA$VEGETATION.DATA$DENSITY_UNIT |> as.character(),
            spt_trait_value = dataRDS$DATA$VEGETATION.DATA$DENSITY |> as.numeric(),
            spt_trait_bexis_id = dataset.id |> as.integer(),
            spt_update = 'NO' |> as.character()
          )
        ) |>
        filter(!is.na(spt_trait_value))
    } else {
      species_traits_data = NA
    }
    
    #### >> 2.12. meta_ts_people ####
    
    meta_ts_people_data <- data.frame(
      # ppl_id = dataRDS$DATA$PEOPLE$ORCID, #TODO to be updated with people id after push
      mts_id = dataRDS$DATA$PEOPLE$META_ID, #TODO extract the list of mts and duplicate rows
      mtp_status = dataRDS$DATA$PEOPLE$STATUS_TIMESERIES |> str_to_upper() |> as.character(),
      mtp_bexis_id = dataset.id |> as.integer(),
      mtp_update = 'NO' |> as.character()
    ) |> 
      mutate(mts_id = str_remove_all(mts_id, pattern = ' ')) |> 
      mutate(start = str_split(mts_id,"-")[[1]][1]) |> 
      mutate(stop = str_split(mts_id,"-")[[1]][2]) |>
      rowwise() |> 
      mutate(mts_id = list(start:stop)) |> 
      unnest(cols = c(mts_id)) |>
      dplyr::select(#mtp_id,
        # ppl_id, #TODO to be updated with people id after push
        mts_id, #TODO extract the list of mts and duplicate rows
        mtp_status,
        mtp_bexis_id,
        mtp_update)
    
    #### >> 2.13. meta_vege_people ####
    if(nrow(dataRDS$DATA$PEOPLE)>0 & 
       nrow(dataRDS$DATA$VEGETATION.METADATA)>0 &
       length(!is.na(dataRDS$DATA$PEOPLE$SURVEY_ID))>0){
      survey_people_data <- data.frame(
        # ppl_id = dataRDS$DATA$PEOPLE$ORCID, #TODO to be updated with people id after push
        #mvs_id = dataRDS$DATA$PEOPLE$SURVEY_ID, #TODO extract the list of mts and duplicate rows
        spl_status = dataRDS$DATA$PEOPLE$STATUS_SPECIES_DATA |> str_to_upper() |> as.character(),
        spl_bexis_id = dataset.id |> as.integer(),
        spl_update = 'NO' |> as.character()
      ) |> 
        mutate(mvs_id = str_remove_all(mvs_id, pattern = ' ')) |> 
        mutate(start = str_split(mvs_id,"-")[[1]][1]) |> 
        mutate(stop = str_split(mvs_id,"-")[[1]][2]) |>
        rowwise() |> 
        mutate(mvs_id = list(start:stop)) |> 
        unnest(cols = c(mvs_id)) |>
        dplyr:: select(
          #spl_id,
          # ppl_id, #TODO to be updated with people id after push
          # mvs_id, #TODO extract the list of mts and duplicate rows
          spl_status,
          spl_bexis_id,
          spl_update)
    }else{
      survey_people_data = NA
    }
  }
  
  #### >> 2.12. Complete datasets
  DATASETS = list(
    people = people_data,
    location = locations_data,
    experiments = experiments_data,
    sites = sites_data,
    loggers = loggers_data,
    meta_ts = meta_ts_data,
    clim_ts = clim_ts_data,
    meta_vega = ifelse(exists(deparse(substitute(meta_vega_surveys_data))),meta_vega_surveys_data,NA),
    veg_surveys_traits_data = ifelse(exists(deparse(substitute(veg_surveys_traits_data))),veg_surveys_traits_data,NA),
    species_data = ifelse(exists(deparse(substitute(species_data))),species_data,NA),
    species_traits_data = ifelse(exists(deparse(substitute(species_traits_data))),species_traits_data,NA),
    meta_st = ifelse(exists(deparse(substitute(meta_ts_people_data))),meta_ts_people_data,NA),
    survey_people = ifelse(exists(deparse(substitute(survey_people_data))),survey_people_data,NA)
  )
  
  return(DATASETS)
}

# To open a dataset: 
# DATASETS = formate.dataset.to.sql(i=1, list.zip.files, listing.bexis, data.path)

