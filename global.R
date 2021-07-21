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

### update data each morning at 3AM
updateTime <<- paste("3",sample(1:59,1),sample(1:59,1),sep =":")

### path for geojsons
rootPath <- '/data'

### load data and set laste update date
loadGlobalData(rootPath)
lastUpdate <<- format(Sys.Date(),"%Y-%m-%d")