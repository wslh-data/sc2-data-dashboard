library(plotly)

#render a plot of the variants of concern
plotVOC <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$DOC,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  
  fig <- plot_ly()
  for(voc in VOC_list){
    data <- data.frame(date="2020-01-01",lineage=voc,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$lineage == voc,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = voc,
      mode = "lines"
    )
  }
  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      ),
      hovermode = 'compare'
    )
  fig
  return(fig)
}