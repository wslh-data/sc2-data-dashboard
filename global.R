library(shiny)
library(plotly)
library(rjson)
library(dplyr)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(stringr)
library(paws)
library(readr)

source("lib/cumulativeSequences.R")
source("lib/plotVariantCurve.R")
source("lib/timeframedata.R")
source("lib/proportionLineageData.R")
source("lib/proportionVariantData.R")
source("lib/sequenceMap.R")
source("lib/dataProcessing.R")
source("lib/retrieve_data.R")
source("lib/valueBoxes.R")
source("lib/county_to_herc.R")

### Get ENV Var if exists
if (file.exists('.env-test')){
  readRenviron(".env-test")
}

### Get Delta Lineage List
lineage_df <- get_lineage_data()
delta_ays <- lineage_df[grepl("AY",lineage_df$lineage),]

### Set VOC/VOI List and WHO Lineages
WHO_list <<- list(
  'Alpha' = c("B.1.1.7","Q.1","Q.2","Q.3","Q.4","Q.5","Q.6",
              "Q.7","Q.8"),
  'Beta' = c("B.1.351","B.1.351.2","B.1.351.3"),
  'Gamma' = c("P.1","P.1.1","P.1.2"),
  'Delta' = c("B.1.617.2",delta_ays),
  'Epsilon' = c("B.1.427","B.1.429"),
  'Eta' = c("B.1.525"),
  'Iota' = c("B.1.526"),
  'Kappa' = c("B.1.617.1"),
  'Lambda' = c("C.37","C.37.1"),
  'Mu' = c("B.1.621","B.1.621.1"),
  'Zeta' = c("P.2")
)

WHO_VOC <<- c(
  'Delta'
)

WHO_VBM <<- c(
  'Alpha',
  'Beta',
  'Gamma',
  'Epsilon',
  'Zeta',
  'Eta',
  'Iota',
  'Kappa',
  'Lambda',
  'Mu'
)

VOC_list <<- WHO_list$Delta

VBM_list <<- c("B.1.617.3", WHO_list$Alpha, WHO_list$Beta, WHO_list$Gamma,
               WHO_list$Epsilon, WHO_list$Eta, WHO_list$Iota, WHO_list$Kappa,
               WHO_list$Mu, WHO_list$Zeta)

### Function to convert lineage to WHO name
getWHO <<- function(x){
  for (i in names(WHO_list)){
    if (x %in% WHO_list[[i]] ){
      return(i)
    }
  }
  return("Other")
}

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
dhsdata <<- get_DHS_county_data("DHS-SC2-County-DataV2")

### load data and set last update date
preProcess()
generateValueBoxPlots()
