library(plotly)

VOC_list = c(
  "B.1.1.7",
  "B.1.351",
  "B.1.351.1",
  "B.1.351.2",
  "B.1.351.3",
  "P.1",
  "P.1.1",
  "P.1.2",
  "B.1.427",
  "B.1.429",
  "B.1.429.1"
)

#render a plot of the variants of concern
plotVOC <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
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
  return(fig)
}