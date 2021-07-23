library(shiny)
library(plotly)
library(rjson)
library(dplyr)
library(shinydashboard)

source("cumulativeSequences.R")
source("plotVOC.R")
source("plotVOI.R")
source("timeframedata.R")
source("proportionLineageData.R")
source("proportionVariantData.R")
source("sequenceMap.R")
source("loadGlobalData.R")

### Set timezone so our update clock makes sense
Sys.setenv(TZ='America/Chicago')

### path for geojsons
rootPath <<- '/data'

### load data and set last update date
loadGlobalData(rootPath)
lastUpdate <<- format(Sys.time(),"%Y-%m-%d")
