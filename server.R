
function(input,output,session) { 
  ### Notifications
  #id <- NULL
  #observe({
  #  if (!is.null(id))
  #    return()
  #  id <<- showNotification(type="warning",paste("Please note the sudden changes in sequencing counts and variant numbers was due to a data transfer issue that was recently identified and corrected. We apologize for the inconvenience."), duration = 20)
  #})
  
  ### Data update date
  output$update_time.c <- output$update_time.b <- output$update_time.a <- renderText({lastUpdate})
  
  #### Value Box
  output$b117vb.c <- output$b117vb.b <-output$b117vb.a <- renderValueBox(b117)
  output$b1351vb.c <- output$b1351vb.b <- output$b1351vb.a <- renderValueBox(b1351)
  output$p1vb.c <- output$p1vb.b <- output$p1vb.a <- renderValueBox(p1)
  output$b16172.c <- output$b16172.b <- output$b16172.a <- renderValueBox(b16172)
  output$b11529.c <- output$b11529.b <- output$b11529.a <- renderValueBox(b11529)
  
  ### Selectable lineage plot
  updateSelectizeInput(session,"selectVariant",choices=sort(unique(sc2Data$Lineage)),server=TRUE)
    
  ### Plot Outputs
  ## Sequencing Report Plots
  #plot number of sequences by time period
  output$sequenceByTimeframe <- renderPlotly(seqFreqPlot(sequenceFreqTimeframe(input$timefreqchoice)))
  #plot cumulative sequence number
  output$totalSequences <- renderPlotly(cumulativeSequences(sc2Data))
  #plot proportion of lineages
  output$lineageByTimeFrame <- renderPlotly(plotTimeLineage(sequenceLineageTimeframe(input$timelinchoice)))
  
  ## Variant Report Plots
  #plot proportion of variants
  output$sequenceVariantByTimeframe <- renderPlotly(plotVariantTimeLineage(sequenceVariantTimeframe(input$timevarchoice),input$labelchoice))
  #plot variants of concern
  output$VOC <- renderPlotly(plotCumulativeVariants(sc2Data,VOC_list))
  #plot variants being monitored
  output$VBM <- renderPlotly(plotCumulativeVariants(sc2Data,VBM_list))
  #plot variants searched
  output$selectVariantByTimeframe <- renderPlotly(plotSelectedLineage(sequenceLineageTimeframe(input$timeselectvarchoice),input$selectVariant))
  
  ## Geographical Report
  #plot variants by herc region
  output$hercVariant <- renderPlotly(plotHERCMap(subsetDataByTime(sc2Data,timerange=input$hercTimeChoice)))
  #plot total sequences by county
  output$countyMap <- renderPlotly(plotCountyMap(sc2Data,dhsdata))
  
  output$downloadAck <- downloadHandler(
    filename = paste(lastUpdate,"_acknowledgements.csv",sep=''),
    content = function(file) {
      write.csv(ackdf,file,row.names = FALSE)
    }
  )

}