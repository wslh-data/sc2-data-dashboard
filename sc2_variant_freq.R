library(plotly)
library(dplyr)

renderTotal <- function(){
  # load SC2 data
  sc2Data = read.csv(Sys.glob(file.path('/data/gisaid_*tsv')),sep="\t")
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
      )
    )
  return(fig)
}

renderVariants <- function(){
  # load SC2 data
  sc2Data = read.csv(Sys.glob(file.path('/data/gisaid_*tsv')),sep="\t")
  sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"),]

  fig <- plot_ly()
  # b.1.1.7
  data <- sc2bylineage[sc2bylineage$lineage == "B.1.1.7",]
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.1.7',
    mode = "lines"
  )

  # b.1.351
  data <- sc2bylineage[sc2bylineage$lineage == "B.1.351",]
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.351',
    mode = "lines"
  )

  # P.1
  data <- sc2bylineage[sc2bylineage$lineage == "P.1",]
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'P.1',
    mode = "lines"
  )

  # P.2
  data <- sc2bylineage[sc2bylineage$lineage == "P.2",]
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'P.2',
    mode = "lines"
  )

  #CAL.20C (B.1.429 & B.1.427)
  data <- merge(sc2bylineage[sc2bylineage$lineage == "B.1.429",], sc2bylineage[sc2bylineage$lineage == "B.1.427",],by="date")
  data$num <- data$num.x + data$num.y
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'CAL.20C (B.1.429 & B.1.427)',
    mode = "lines"
  )

  # B.1.525
  data <- sc2bylineage[sc2bylineage$lineage == "B.1.525",]
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(data$date, format= "%Y-%m-%d"),
    y = cumsum(data$num),
    name = 'B.1.525',
    mode = "lines"
  )

  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      )
    )
  return(fig)
}
