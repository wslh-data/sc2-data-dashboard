
function(input,output,session) { 
  ### Data update date
  output$update_time.c <- output$update_time.b <- output$update_time.a <- renderText({lastUpdate})
  
  #### Value Box
  output$b117vb.c <- output$b117vb.b <-output$b117vb.a <- renderValueBox(b117)
  output$b1351vb.c <- output$b1351vb.b <- output$b1351vb.a <- renderValueBox(b1351)
  output$p1vb.c <- output$p1vb.b <- output$p1vb.a <- renderValueBox(p1)
  output$b16172.c <- output$b16172.b <- output$b16172.a <- renderValueBox(b16172)
  
  ### Selectable lineage plot
  updateSelectizeInput(session,"selectVariant",choices=sort(unique(sc2Data$Lineage)),server=TRUE)
    
  ### Plot Outputs
  output$totalSequences <- renderPlotly(totalseqplot)
  output$sequenceByTimeframe <- renderPlotly(seqFreqPlot(sequenceFreqTimeframe(input$timefreqchoice)))
  output$sequenceVariantByTimeframe <- renderPlotly(plotVariantTimeLineage(sequenceVariantTimeframe(input$timevarchoice)))
  output$selectVariantByTimeframe <- renderPlotly(plotSelectedLineage(sequenceLineageTimeframe(input$timeselectvarchoice),input$selectVariant))
  output$lineageByTimeFrame <- renderPlotly(plotTimeLineage(sequenceLineageTimeframe(input$timelinchoice)))
  output$VOC <- renderPlotly(vocplot)
  output$VOI <- renderPlotly(voiplot)
  output$hercVariant <- renderPlotly(plotHERCMap(sc2Data,herc_geojson,input$herctimechoice))
  output$countyMap <- renderPlotly(countyMapPlot)
  
  output$downloadAck <- downloadHandler(
    filename = paste(lastUpdate,"_acknowledgements.csv",sep=''),
    content = function(file) {
      write.csv(ackdf,file,row.names = FALSE)
    }
  )

}