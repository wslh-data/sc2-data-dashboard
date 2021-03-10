library(plotly)
library(dplyr)

renderTotal <- function(sc2Data){
  # load SC2 data
  sc2byDate <- data.frame(table(sc2Data$Collection.date))
  names(sc2byDate) <- c("date","num")
  sc2byDate <- sc2byDate[!(sc2byDate$date=="2020"),]

  fig <- plot_ly()
  # total number
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(sc2byDate$date, format= "%Y-%m-%d"),
    y = cumsum(sc2byDate$num),
    name = 'Total',
    mode = "lines"
  )
  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      ),
      hovermode = 'compare',
      autosize=TRUE
    )
  return(fig)
}
renderVOC <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"),]

  fig <- plot_ly()
  # b.1.1.7
  data <- data.frame(date="2020-01-01",lineage="B.1.1.7",num=0)
  data <- rbind(sc2bylineage[sc2bylineage$lineage == "B.1.1.7",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.1.7',
    mode = "lines"
  )

  # b.1.351
  data <- data.frame(date="2020-01-01",lineage="B.1.351",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "B.1.351",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.351',
    mode = "lines"
  )

  # P.1
  data <- data.frame(date="2020-01-01",lineage="P.1",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "P.1",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'P.1',
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

renderVOI <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"),]
  
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
  
  #CAL.20C (B.1.429 & B.1.427)
  data <- data.frame(date="2020-01-01",lineage.x="B.1.429",num.x=0,lineage.y="B.1.427",num.y=0)
  data <- rbind(data,merge(sc2bylineage[sc2bylineage$lineage == "B.1.429",], sc2bylineage[sc2bylineage$lineage == "B.1.427",],by="date"))
  data$num <- data$num.x + data$num.y
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'CAL.20C (B.1.429 & B.1.427)',
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

  # A.23.1
  data <- data.frame(date="2020-01-01",lineage="A.23.1",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "A.23.1",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'A.23.1',
    mode = "lines"
  )
  
  # B.1.1.318
  data <- data.frame(date="2020-01-01",lineage="B.1.1.318",num=0)
  data <- rbind(data,sc2bylineage[sc2bylineage$lineage == "B.1.1.318",])
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.1.318',
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