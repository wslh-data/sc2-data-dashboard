library(plotly)

VOI_list = c(
  "B.1.525",
  "B.1.526",
  "B.1.526.1",
  "B.1.617",
  "B.1.617.1",
  "B.1.617.2",
  "B.1.617.3",
  "P.2"
)

#render a plot of the variants of interest
plotVOI <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  
  fig <- plot_ly()
  for(voi in VOI_list){
    data <- data.frame(date="2020-01-01",lineage=voi,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$lineage == voi,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = voi,
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