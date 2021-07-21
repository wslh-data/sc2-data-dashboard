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

rootPath <- '/data'

loadGlobalData(rootPath)
lastUpdate <<- format(Sys.Date(),"%Y-%m-%d")
