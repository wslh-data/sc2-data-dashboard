library(shiny)
library(plotly)
library(rjson)
library(dplyr)
library(shinydashboard)
library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(stringr)

source("cumulativeSequences.R")
source("plotVOC.R")
source("plotVOI.R")
source("timeframedata.R")
source("proportionLineageData.R")
source("proportionVariantData.R")
source("sequenceMap.R")
source("dataProcessing.R")
source("retrieve_data.R")
source("valueBoxes.R")

### Set VOC/VOI List
VOI_list <<- c(
  "B.1.525",
  "B.1.526",
  "C.37",
  "B.1.617.1",
  "B.1.617.3"
)

VOC_list <<- c(
  "B.1.1.7",
  "B.1.351",
  "B.1.351.2",
  "B.1.351.3",
  "B.1.617.2",
  "AY.1",
  "AY.2",
  "AY.3",
  "AY.3.1",
  "AY.4",
  "AY.5",
  "AY.6",
  "AY.7",
  "AY.8",
  "AY.9",
  "AY.10",
  "AY.11",
  "AY.12",
  "P.1",
  "P.1.1",
  "P.1.2"
)

### Set timezone so our update clock makes sense
Sys.setenv(TZ='America/Chicago')
lastUpdate <<- format(Sys.time(),"%Y-%m-%d")

### load data
# GeoJSON Files
WICounty_geojson <<- fromJSON(file=file.path("geojsons","us-counties-fips.json"))
HERC_geojson <<- fromJSON(file=file.path("geojsons","WI_HERC.json"))

# GISAID Data
sc2Data <<- get_GISAID_Metadata_data()
sc2Data <<- unique(sc2Data,by="GISAID_ID")
sc2Data <<- sc2Data[!is.na(sc2Data$GISAID_ID),]

# DHS Data
dhsdata <<- get_DHS_county_data()

### load data and set last update date
preProcess()
generateValueBoxPlots()
