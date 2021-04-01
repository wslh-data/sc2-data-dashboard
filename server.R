library(shiny)
library(plotly)
library(rjson)
library(dplyr)

source("cumulativeSequences.R")
source("plotVOC.R")
source("plotVOI.R")
source("timeframedata.R")
source("proportionLineageData.R")
source("proportionVariantData.R")
source("sequenceMap.R")

### Load data
sc2Data = read.csv(Sys.glob(file.path('./data/gisaid_*tsv')),sep="\t")
dhsdata = read.csv("./data/County_Table_data.csv")

### GeoJSON Files
WICounty_geojson <- fromJSON(file="./data/geojson-counties-fips.json")
herc_geojson <- fromJSON(file="./data/Wisconsin_Healthcare_Emergency_Readiness_Coalition_Regions.json")

############################
##### Pre-analize Data #####
############################

#### Sequence Frequency by Time Period
timeFrameData <- prepareTimeFrameData(sc2Data)

sequenceFreqTimeframe <- function(choice){
  if(choice == "Weekly"){
    return(timeFrameData$weekly)
  }
  else if(choice == "Quarterly"){
    return(timeFrameData$quarterly)
  }
  else{
    return(timeFrameData$monthly)
  }
}

#### Proportion of Lineages Sequenced by Time Period

# lineagePropData <- prepareLineagePropData(sc2Data)
# 
# sequenceLineageTimeframe <- function(choice){
#   if(choice == "Weekly"){
#     return(lineagePropData$weekly)
#   }
#   else if(choice == "Quarterly"){
#     return(lineagePropData$quarterly)
#   }
#   else{
#     return(lineagePropData$monthly)
#   }
# }

#### Proportion of Variants Sequenced by Time Period

variantPropData <- prepareVariantPropData(sc2Data)

sequenceVariantTimeframe <- function(choice){
  if(choice == "Weekly"){
    return(variantPropData$weekly)
  }
  else if(choice == "Quarterly"){
    return(variantPropData$quarterly)
  }
  else{
    return(variantPropData$monthly)
  }
}

#### Generate Plots
voiplot <- plotVOI(sc2Data)
vocplot <- plotVOC(sc2Data)
totalseqplot <- cumulativeSequences(sc2Data)
countyMapPlot <- plotCountyMap(sc2Data,dhsdata,WICounty_geojson)
hercMapPlot <- plotHERCMap(sc2Data,dhsdata,herc_geojson)


############################
############################
############################



function(input,output,session) { 
    
  ### Plot Outputs
  output$totalSequences <- renderPlotly(totalseqplot)
  output$sequenceByTimeframe <- renderPlotly(seqFreqPlot(sequenceFreqTimeframe(input$timefreqchoice)))
  output$sequenceVariantByTimeframe <- renderPlotly(plotVariantTimeLineage(sequenceVariantTimeframe(input$timevarchoice)))
  output$VOC <- renderPlotly(vocplot)
  output$VOI <- renderPlotly(voiplot)
  output$hercVariant <- renderPlotly(hercMapPlot)
  output$countyMap <- renderPlotly(countyMapPlot)

}