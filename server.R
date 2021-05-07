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

### Root Data Path

rootPath <- '/data'

loadGlobalData(rootPath)


function(input,output,session) { 
  
  ### refresh data every hour
  autoInvalidate <- reactiveTimer(3600000)
  observe({
    autoInvalidate()
    loadGlobalData(rootPath)
  })

  ### Data update date
  output$update_time.c <- output$update_time.b <- output$update_time.a <- renderText({update_time})
  
  #### Value Box
  output$b117vb.c <- output$b117vb.b <-output$b117vb.a <- renderValueBox(b117)
  output$b1351vb.c <- output$b1351vb.b <- output$b1351vb.a <- renderValueBox(b1351)
  output$p1vb.c <- output$p1vb.b <- output$p1vb.a <- renderValueBox(p1)
  output$b1429b1427.c <- output$b1429b1427.b <- output$b1429b1427.a <- renderValueBox(b1429b1427)
  
  ### Selectable lineage plot
  updateSelectizeInput(session,"selectVariant",choices=sort(unique(sc2Data$Lineage)),server=TRUE)
    
  ### Plot Outputs
  output$totalSequences <- renderPlotly(totalseqplot)
  output$sequenceByTimeframe <- renderPlotly(seqFreqPlot(sequenceFreqTimeframe(input$timefreqchoice)))
  output$sequenceVariantByTimeframe <- renderPlotly(plotVariantTimeLineage(sequenceVariantTimeframe(input$timevarchoice)))
  output$selectVariantByTimeframe <- renderPlotly(plotSelectedLineage(sequenceLineageTimeframe(input$timevarchoice),input$selectVariant))
  output$lineageByTimeFrame <- renderPlotly(plotTimeLineage(sequenceLineageTimeframe(input$timelinchoice)))
  output$VOC <- renderPlotly(vocplot)
  output$VOI <- renderPlotly(voiplot)
  output$hercVariant <- renderPlotly(plotHERCMap(sc2Data,dhsdata,herc_geojson,input$herctimechoice))
  output$countyMap <- renderPlotly(countyMapPlot)
  
  output$downloadAck <- downloadHandler(
    filename = "gisaid_acknowledgements.pdf",
    content = function(file) {
      file.copy(ackfile, file)
    }
  )

}