rm(list = ls())
library(jsonlite)
library(XML)
library(xml2)
library(curl)
library(tidyverse)

userpwd = getPass::getPass(msg = 'USER:PWD')
setwd('0-data/bexis_download/')

h <- new_handle()
handle_setopt(
  handle = h,
  httpauth = 1,
  userpwd = userpwd
)

# data.frame(bexis.max = 0, downloaded = 0, processed = 0) |>
#   write.csv('processing-records.csv', row.names = F)

df.process = read.csv(file = 'processing-records.csv')

# Identify the datasets available
list.of.datasets <- fromJSON("https://database.soilbon.org/api/Metadata")
list.of.attachment <- fromJSON("https://database.soilbon.org/api/Attachment/")$Attachments

# Identify datasets with SoilTemp Data
listing.bexis = 
  map_df(.x = 1:nrow(list.of.datasets),
    .f = ~{
      resp = curl:: curl_fetch_memory(
        url = paste0("https://database.soilbon.org/api/Metadata/", list.of.datasets$DatasetId[.x]), 
        handle = h)
      
      L = resp$content |> 
        rawToChar() |> 
        read_xml() |> 
        xmlParse() |> 
        xmlToList()
      
      L.attachments = list.of.attachment[[.x]]
      
      tibble(
        dataset = list.of.datasets$DatasetId[.x],
        DOI = L$general$generalType$DOI$DOIType['number'],
        project = L$general$generalType$projectTitle$projectTitleType$text,
        attachment.id = L.attachments$Id,
        attachment.name = L.attachments$Name)
    })

MDB.listing = 
  listing.bexis |>
  filter(!is.na(project) & project == 'SoilTemp') |> 
  filter(!is.na(attachment.id))

write.csv(MDB.listing, 'listing-bexis.csv', row.names = F)

df.process$bexis.max = max(MDB.listing$dataset)
write.csv(df.process, file = 'processing-records.csv', row.names = F)

# Download all the pdf reports
pdf.files = MDB.listing[grep('.pdf', MDB.listing$attachment.name),]
map(.x = (nrow(pdf.files[pdf.files$dataset<=df.process$downloaded,])+1):nrow(pdf.files),
    .f = ~{
      curl_download(url = paste0("https://database.soilbon.org/api/Attachment/", 
                                 pdf.files$dataset[.x], '/',
                                 pdf.files$attachment.id[.x]
                                 ),
                         destfile = paste0('dataset-', pdf.files$dataset[.x], '_report.pdf'),
                         handle = h)
      }
    )

# Downloading all datasets from BEXIS
zip.files = MDB.listing[grep('.zip', MDB.listing$attachment.name),]
map(.x = (nrow(zip.files[zip.files$dataset<=df.process$downloaded,])+1):nrow(zip.files),
    .f = ~{
      curl_download(url = paste0("https://database.soilbon.org/api/Attachment/", 
                                 zip.files$dataset[.x], '/',
                                 zip.files$attachment.id[.x]
      ),
      destfile = paste0('dataset-', zip.files$dataset[.x], 
                        ifelse(grepl('BEXIS',zip.files$attachment.name[.x]),
                           '_BEXIS.zip',
                           '_data.zip') 
                          ),
      handle = h)
    }
)

df.process$downloaded = max(zip.files$dataset)
write.csv(df.process, file = 'processing-records.csv', row.names = F)