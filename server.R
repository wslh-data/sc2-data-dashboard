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

### Load data
sc2Data = read.csv(Sys.glob(file.path('/data/gisaid_*tsv')),sep="\t")
dhsdata = read.csv("/data/County_Table_data.csv")
ackfile = Sys.glob(file.path("/data/gisaid_hcov-19_acknowledgement_table_*.pdf"))

### GeoJSON Files
WICounty_geojson <- fromJSON(file="/data/geojson-counties-fips.json")
herc_geojson <- fromJSON(file="/data/Wisconsin_Healthcare_Emergency_Readiness_Coalition_Regions.json")

############################
##### Pre-analyze Data #####
############################

#### Remove last 2 weeks of data from sc2Data
date_filter <- as.Date(sc2Data$Collection.date) < (Sys.Date() - 21)
sc2Data <- sc2Data[date_filter,]

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

lineagePropData <- prepareLineagePropData(sc2Data)

sequenceLineageTimeframe <- function(choice){
  if(choice == "Weekly"){
    return(lineagePropData$weekly)
  }
  else if(choice == "Monthly"){
    return(lineagePropData$monthly)
  }
  else{
    return(lineagePropData$quarterly)
  }
}

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


#### Variant Value Box
b117 <- valueBox(
  value = nrow(sc2Data[sc2Data$Lineage == "B.1.1.7",]),
  subtitle = "B.1.1.7",
  icon = icon("virus"),
  width = NULL,
  color = "orange",
  href = "https://outbreak.info/situation-reports?pango=B.1.1.7"
)

b1351 <- valueBox(
  value = nrow(sc2Data[sc2Data$Lineage == "B.1.351",]),
  subtitle = "B.1.351",
  icon = icon("virus"),
  width = NULL,
  color = "orange",
  href = "https://outbreak.info/situation-reports?pango=B.1.351"
)

p1 <- valueBox(
  value = nrow(sc2Data[sc2Data$Lineage == "P.1",]),
  subtitle = "P.1",
  icon = icon("virus"),
  width = NULL,
  color = "orange",
  href = "https://outbreak.info/situation-reports?pango=P.1"
)

############################
############################
############################






function(input,output,session) { 
  
  #### Value Box
  output$b117vb.c <- output$b117vb.b <-output$b117vb.a <- renderValueBox(b117)
  output$b1351vb.c <- output$b1351vb.b <- output$b1351vb.a <- renderValueBox(b1351)
  output$p1vb.c <- output$p1vb.b <- output$p1vb.a <- renderValueBox(p1)
    
  ### Plot Outputs
  output$totalSequences <- renderPlotly(totalseqplot)
  output$sequenceByTimeframe <- renderPlotly(seqFreqPlot(sequenceFreqTimeframe(input$timefreqchoice)))
  output$sequenceVariantByTimeframe <- renderPlotly(plotVariantTimeLineage(sequenceVariantTimeframe(input$timevarchoice)))
  output$lineageByTimeFrame <- renderPlotly(plotTimeLineage(sequenceLineageTimeframe(input$timelinchoice)))
  output$VOC <- renderPlotly(vocplot)
  output$VOI <- renderPlotly(voiplot)
  output$hercVariant <- renderPlotly(hercMapPlot)
  output$countyMap <- renderPlotly(countyMapPlot)
  
  ### Modal about the data
  # shinyjs::disable("about_data")
  # observe({
  #   if (input$navtabs == "About the Data")  {
  #     showModal(modalDialog(
  #       title = "Important message",
  #       div(id = "aa", style = "width: 1100px; height: 100px;", HTML("<b>This is </b>an important message!")),
  #       easyClose = TRUE
  #     ))
  #   }
  # })
  
  output$downloadAck <- downloadHandler(
    filename = "gisaid_acknowledgements.pdf",
    content = function(file) {
      file.copy(ackfile, file)
    }
  )

}