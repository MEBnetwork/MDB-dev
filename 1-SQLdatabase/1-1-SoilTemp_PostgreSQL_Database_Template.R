# ************************************ 
# Create MDB database PostgreSQL
# ************************************

# Load required libraries
rm(list = ls())
library(DBI)
library(RPostgres)
library(getPass)

# @Bedassa Set your credentials below 

# dsn_database = "MDB"   #  name of your Database
# dsn_hostname = "postgres02.srv.idiv.de"  
# dsn_port = 5432
# dsn_uid = "dbfKN5Xyr1"
# dsn_pwd = "z%Ri84s%$s<_8GU^"

## Connect the Database with R using RPostgreSQL
tryCatch({
  drv <- RPostgres::Postgres()
  print("Connecting to Database...")
  connec <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
  print("Database Connected!")
}, 
  error = function(cond) {
  print("Unable to connect to Database.")
})

# Check tables setting 
dbListTables(connec)

# Condition
tables <- dbListTables(connec)
user_input <- readline(prompt = "Do you want to drop tables? (true/false): ")
drop_tables <- tolower(user_input) %in% c("true", "yes")
if (drop_tables) {
  for (table_name in tables) {
    drop_query <- paste("DROP TABLE IF EXISTS", table_name, "CASCADE ")
    dbSendQuery(connec, drop_query)
  }
  message("Tables dropped successfully.")
} else {
  message("Tables not dropped.")
}

# Check tables
dbListTables(connec)

##################### 1. locations #########################
create_table_loc <- "
CREATE TABLE locations (
    loc_long DECIMAL(9,6) NOT NULL CHECK (loc_long >= -180 AND loc_long <= 180),
    loc_lat DECIMAL(8,6) NOT NULL CHECK (loc_lat >= -90 AND loc_lat <= 90),
    loc_alt DECIMAL(8,2) CHECK (loc_alt >= -10000 AND loc_alt <= 10000),
    loc_country CHAR(2),
    loc_update VARCHAR(24),
    PRIMARY KEY (loc_long,loc_lat)
);

"
dbExecute(connec, create_table_loc)

################################ 2. experiments ################################ 
create_table_exp <- "
CREATE TABLE experiments (
    exp_id SERIAL PRIMARY KEY,
    exp_name VARCHAR(200) NOT NULL,
    exp_manip BOOLEAN,
    exp_insitu BOOLEAN,
    exp_clim_manip BOOLEAN,
    exp_citizen BOOLEAN,
    exp_design VARCHAR(10000),
    exp_doi VARCHAR(500),
    exp_bexis_id INTEGER NOT NULL,
    exp_comment VARCHAR(5000),
    exp_update VARCHAR(24)
);
"
dbExecute(connec, create_table_exp)

################################ 3. site_flags #################################
create_table_site_flags <- "
CREATE TABLE site_flags (
    sif_id INTEGER PRIMARY KEY,
    sif_combination VARCHAR(500),
    sif_description VARCHAR(5000)
);
"
dbExecute(connec, create_table_site_flags)

################################### 4. sites ###################################
create_table_sites <- "
CREATE TABLE sites (
    site_id SERIAL PRIMARY KEY,
    exp_id INTEGER NOT NULL,
    loc_long NUMERIC NOT NULL,
    loc_lat NUMERIC NOT NULL,
    sif_id INTEGER NOT NULL,
    site_owner_id VARCHAR(200) NOT NULL,
    site_spatial_res INTEGER,
    site_long NUMERIC CHECK (site_long >= -180 AND site_long <= 180) NOT NULL,
    site_lat NUMERIC CHECK (site_lat >= -180 AND site_lat <= 180) NOT NULL,
    site_alt NUMERIC CHECK (site_alt >= -10000 AND site_alt <= 10000),
    site_habitat INTEGER CHECK (site_habitat >= 1 AND site_habitat <= 18),
    site_subhabitat INTEGER CHECK (site_subhabitat >= 1 AND site_subhabitat <= 18),
    site_bexis_id INTEGER NOT NULL,
    site_comment VARCHAR(5000),
    site_update VARCHAR(24),
    FOREIGN KEY (exp_id) REFERENCES experiments(exp_id),
    FOREIGN KEY (loc_long,loc_lat) REFERENCES locations(loc_long,loc_lat),
    FOREIGN KEY (sif_id) REFERENCES site_flags(sif_id)
);
"
dbExecute(connec, create_table_sites)
################################ 5. logger_flags ###############################
create_table_logger_flags <- "
CREATE TABLE logger_flags (
    lgf_id INTEGER PRIMARY KEY,
    lgf_combination VARCHAR(5000),
    lgf_description VARCHAR(500)
);
"
dbExecute(connec, create_table_logger_flags)

################################## 6. loggers ##################################
create_table_loggers <- "
CREATE TABLE loggers (
    log_id SERIAL PRIMARY KEY,
    lgf_id INTEGER NOT NULL,
    log_owner_id VARCHAR(50),
    log_serial_number VARCHAR(50) NOT NULL,
    log_brand VARCHAR(20) NOT NULL,
    log_type VARCHAR(20) NOT NULL,
    log_age VARCHAR(10),
    log_bexis_id INTEGER NOT NULL,
    log_comment VARCHAR(5000),
    log_update VARCHAR(24),
    FOREIGN KEY (lgf_id) REFERENCES logger_flags(lgf_id)
);
"
dbExecute(connec, create_table_loggers)

############################### 7. meta_ts_flags ###############################
create_table_meta_ts_flags <- "
CREATE TABLE meta_ts_flags (
    mtf_id INTEGER PRIMARY KEY,
    mtf_combination VARCHAR(5000),
    mtf_description VARCHAR(500)
);
"
dbExecute(connec, create_table_meta_ts_flags)

################################## 8. meta_ts ##################################
create_table_meta_ts <- "
CREATE TABLE meta_ts (
    mts_id SERIAL PRIMARY KEY,
    site_id INTEGER NOT NULL,
    log_id INTEGER NOT NULL,
    mtf_id INTEGER NOT NULL,
    mts_owner_id VARCHAR(50),
    mts_sensor_code VARCHAR(50) NOT NULL,
    mts_sensor_shielding BOOLEAN,
    mts_homemade_shield BOOLEAN,
    mts_clim_variable TEXT NOT NULL,
    mts_clim_unit TEXT,
    mts_clim_accuracy TEXT,
    mts_clim_temporal_res TEXT,
    mts_sensor_height NUMERIC CHECK (mts_sensor_height >= -9999 AND mts_sensor_height <= 9999),-- Height of the sensor (in cm)
    mts_sensor_height_range NUMERIC CHECK (mts_sensor_height_range >= -9999 AND mts_sensor_height_range <= 9999),-- Height of the sensor (in cm)
    mts_timezone_utc_offset NUMERIC CHECK (mts_timezone_utc_offset >= -12 AND mts_timezone_utc_offset <= 12),
    mts_date_start DATE,
    mts_date_stop DATE,
    mts_licence VARCHAR(50),
    mts_doi VARCHAR(50),
    mts_bexis_id INTEGER NOT NULL,
    mts_ts_comment VARCHAR(5000),
    mts_update VARCHAR(24),
    FOREIGN KEY (site_id) REFERENCES sites(site_id),
    FOREIGN KEY (log_id) REFERENCES loggers(log_id),
    FOREIGN KEY (mtf_id) REFERENCES meta_ts_flags(mtf_id)
);
"
dbExecute(connec, create_table_meta_ts)

############################# 9. meta_veg_surveys ##############################
create_table_meta_veg_surveys <- "
CREATE TABLE meta_veg_surveys (
   mvs_id SERIAL PRIMARY KEY,
    site_id INT NOT NULL,
    mvs_date DATE,
    mvs_plot_size NUMERIC CHECK (mvs_plot_size >= 0 AND mvs_plot_size <= 1000),
    mvs_method_short VARCHAR(100),
    mvs_method_long VARCHAR(500),
    mvs_tax_ref VARCHAR(50),
    mvs_multilayer_vegetation BOOLEAN,
    mvs_other varchar(500),
    mvs_unit VARCHAR(50),
    mvs_doi VARCHAR(500),
    mvs_licence license_type,
    mvs_bexis_id INTEGER NOT NULL,
    mvs_comment VARCHAR(50),
    mvs_update VARCHAR(24),
    FOREIGN KEY (site_id) REFERENCES sites(site_id)
);
"
dbExecute(connec, create_table_meta_veg_surveys)
################################ 10. species_flags #############################
create_table_species_flags <- "
CREATE TABLE species_flags (
    spf_id INTEGER PRIMARY KEY,
    spf_combination VARCHAR(500),
    spf_description VARCHAR(5000)
);
"
dbExecute(connec, create_table_species_flags)

################################# 11. species ##################################
create_table_species <- "
CREATE TABLE species (
    spc_id SERIAL PRIMARY KEY,
    mvs_id INTEGER NOT NULL,
    spf_id INTEGER NOT NULL,
    spc_name VARCHAR(100) NOT NULL,
    spc_bexis_id INTEGER NOT NULL,
    spc_comment VARCHAR(5000),
    spc_update VARCHAR(24),
    FOREIGN KEY (mvs_id) REFERENCES meta_veg_surveys(mvs_id),
    FOREIGN KEY (spf_id) REFERENCES species_flags(spf_id)
);
"
dbExecute(connec, create_table_species)

############################# 12. species_traits_flags #########################
create_table_species_traits_flags <- "
CREATE TABLE species_traits_flags (
    stf_id INTEGER PRIMARY KEY,
    stf_combination VARCHAR(500),
    stf_description VARCHAR(5000)
);
"
dbExecute(connec, create_table_species_traits_flags)

############################### 13. species_traits #############################
create_table_species_traits <- "
CREATE TABLE species_traits (
    spt_id SERIAL PRIMARY KEY,
    spc_id INT NOT NULL,
    stf_id INT NOT NULL,
    spt_trait VARCHAR(50),
    spt_trait_unit VARCHAR(10),
    spt_trait_val NUMERIC CHECK (spt_trait_val >= 0 AND spt_trait_val <= 999),
    spt_trait_bexis_id INTEGER NOT NULL,
    spt_update VARCHAR(24),
    FOREIGN KEY (spc_id) REFERENCES species(spc_id),
    FOREIGN KEY (stf_id) REFERENCES species_traits_flags(stf_id)
);
"
dbExecute(connec, create_table_species_traits)

################################ 14. veg_surveys_flags #########################
create_table_veg_surveys_flags <- "
CREATE TABLE veg_surveys_flags (
    vsf_id INTEGER PRIMARY KEY,
    vsf_combination VARCHAR(500),
    vsf_description VARCHAR(5000)
);
"
dbExecute(connec, create_table_veg_surveys_flags)

################################ 15. veg_surveys_traits ########################
create_table_veg_surveys_traits <- "
CREATE TABLE veg_surveys_traits (
    vst_id SERIAL PRIMARY KEY,
    vsf_id INTEGER NOT NULL,
    mvs_id INTEGER NOT NULL,
    vst_trait VARCHAR(50),
    vst_trait_unit VARCHAR(10),
    vst_trait_value NUMERIC CHECK (vst_trait_value >= 0 AND vst_trait_value <= 999),
    vst_trait_bexis_id INTEGER NOT NULL,
    vst_update VARCHAR(10),
    FOREIGN KEY (vsf_id) REFERENCES veg_surveys_flags(vsf_id),
    FOREIGN KEY (mvs_id) REFERENCES meta_veg_surveys(mvs_id)
);
"
dbExecute(connec, create_table_veg_surveys_traits)

################################## 16. people ##################################
create_table_people <- "
CREATE TABLE people (
    ppl_id SERIAL PRIMARY KEY,
    ppl_firstname VARCHAR(200),
    ppl_middlename_initials VARCHAR(100),
    ppl_lastname VARCHAR(200) NOT NULL,
    ppl_coauthor_name VARCHAR(200) NOT NULL,
    ppl_email VARCHAR(200) NOT NULL,
    ppl_second_email VARCHAR(200),
    ppl_affiliation VARCHAR(500),
    ppl_address VARCHAR(500),
    ppl_orcid VARCHAR(50),
    ppl_bexis_id INTEGER NOT NULL,
    ppl_soiltemp_update BOOLEAN,
    ppl_update VARCHAR(10)
);
"
dbExecute(connec, create_table_people)

################################# 17. survey_people ############################
create_table_survey_people <- "
CREATE TABLE survey_people (
    spl_id SERIAL PRIMARY KEY,
    ppl_id INTEGER NOT NULL,
    mvs_id INTEGER NOT NULL,
    spl_status VARCHAR(100) NOT NULL,
    spl_bexis_id INTEGER NOT NULL,
    spl_update VARCHAR(10),
    FOREIGN KEY (mvs_id) REFERENCES meta_veg_surveys(mvs_id),
    FOREIGN KEY (ppl_id) REFERENCES people(ppl_id)
);
"
dbExecute(connec, create_table_survey_people)

################################ 18. meta_ts_people ############################
create_table_meta_ts_people <- "
CREATE TABLE meta_ts_people (
    mtp_id SERIAL NOT NULL,
    ppl_id INTEGER NOT NULL,
    mts_id INTEGER NOT NULL,
    mtp_status VARCHAR(50) NOT NULL,
    mtp_bexis_id INTEGER NOT NULL,
    mtp_update VARCHAR(10),
    PRIMARY KEY (mtp_id , mts_id, ppl_id),
    FOREIGN KEY (mts_id) REFERENCES meta_ts(mts_id),
    FOREIGN KEY (ppl_id) REFERENCES people(ppl_id)
);
"
dbExecute(connec, create_table_meta_ts_people)

################################# 19. clim_ts_flags ############################
create_table_clim_ts_flags <- "
CREATE TABLE clim_ts_flags (
    ctf_id SERIAL PRIMARY KEY,
    ctf_combination VARCHAR(500),
    ctf_description VARCHAR(5000)
);
"
dbExecute(connec, create_table_clim_ts_flags)

###################################### 20. clim_ts #############################
create_table_clim_ts <- "
CREATE TABLE clim_ts (
    cts_id SERIAL PRIMARY KEY,
    mts_id INTEGER NOT NULL,
    ctf_id INTEGER NOT NULL,
    cts_timestamp_utc TEXT,
    cts_value NUMERIC CHECK (cts_value >= -1000000000 AND cts_value <= 1000000000),
    cts_sensor_id VARCHAR(50),
    cts_bexis_id INTEGER NOT NULL,
    cts_update VARCHAR(24),
    FOREIGN KEY (mts_id) REFERENCES meta_ts(mts_id),
    FOREIGN KEY (ctf_id) REFERENCES clim_ts_flags(ctf_id)
);
"
dbExecute(connec, create_table_clim_ts)

########################## Modeling the database structure ##################### 
# Visualization
library(dplyr)

dbListTables(connec)

dbListTables(connec) %>%
  purrr::map(
    ~ dbExecute(connec, paste0('GRANT ALL ON "', .x, '" TO "SoilTemp_editors"'))
  )

library(DiagrammeR)
library(datamodelr)

library(rsvg)

dm_soiltemp = connec %>% 
  dbGetQuery(., 
             dm_re_query("postgres")) %>% 
  as.data_model() %>%
  dm_set_segment(.,
                 list(People = c("people", 'meta_ts_people', 'survey_people'),
                      Metadata = c('locations', 'experiments', 'sites','site_flags'),
                      Temperature = c('meta_ts', 'loggers', 'clim_timeseries','logger_flags','meta_ts_flags', 'clim_ts_flags'),
                      Vegetation = c('survey_flags', 'species_flags', 'veg_surveys_flags',
                                     'meta_veg_surveys', 'species_traits','veg_surveys_traits', 'species'))) %>%
  dm_set_display(.,
                 list(accent1nb = c("people", 'meta_ts_people', 'survey_people'),
                      accent2nb = c('locations', 'experiments', 'sites'),
                      accent3nb = c('meta_ts', 'loggers', 'clim_ts'),
                      accent4nb = c('meta_veg_surveys','veg_surveys_traits', 'species_traits', 'species'),
                      accent6nb = c('site_flags', 'logger_flags', 'meta_ts_flags', 'clim_ts_flags', 'survey_flags', 'species_flags','veg_surveys_flags','species_traits_flags')))

# remove duplicated actifacts
dm_soiltemp$columns = dm_soiltemp$columns %>% select(-ref_col) %>% distinct_all()


graph = dm_create_graph(dm_soiltemp,
                        rankdir = 'BT',
                        graph_attrs = "rankdir = RL, bgcolor = '#F4F0EF' ", 
                        edge_attrs = "dir = both, arrowtail = crow, arrowhead = odiamond",
                        node_attrs = "fontname = 'Arial'")


graph

# dm_export_graph(graph, 
#                 file_name = paste0('2-code/2-4-SQLdatabase/2-4-1-',Sys.Date(),'_SoilTemp_database-structure.png'), 
#                 file_type = 'PNG')
# 
# dm_export_graph(graph, 
#                 file_name = paste0('2-code/2-4-SQLdatabase/2-4-1-',Sys.Date(),'_SoilTemp_database-structure.svg'), 
#                 file_type = 'SVG')

# Close the database connection
dbDisconnect(connec)