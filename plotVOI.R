library(plotly)

#render a plot of the variants of interest
plotVOI <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  
  fig <- plot_ly()
  # P.2
  data <- data.frame(date="2020-01-01",lineage="P.2",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "P.2",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'P.2',
    mode = "lines"
  )
  
  # B.1.525
  data <- data.frame(date="2020-01-01",lineage="B.1.525",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "B.1.525",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.525',
    mode = "lines"
  )
  
  # B.1.526
  data <- data.frame(date="2020-01-01",lineage="B.1.526",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "B.1.526",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.526',
    mode = "lines"
  )
  
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