library(plotly)

prepareTimeFrameData <- function(data){
  ### Sequence Frequencies by Timeframe
  sc2byDate <- data.frame(table(data$Collection.date))
  names(sc2byDate) <- c("date","num")
  sc2byDate <- sc2byDate[!(sc2byDate$date=="2020"|sc2byDate$date=="2021"),]
  sc2byDate$date <- as.Date(sc2byDate$date, format= "%Y-%m-%d")
  sc2byDate <- within(sc2byDate, {
    weeks <- format(date, "%U-%Y")
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(date), format(date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  sc2byDate <- sc2byDate[!is.na(sc2byDate$date),]
  sc2byDate <- droplevels(sc2byDate)
  
  weekly <- sc2byDate %>% group_by(weeks) %>% summarise(num = sum(num))
  names(weekly) <- c("date","num")
  monthly <- sc2byDate %>% group_by(months) %>% summarise(num = sum(num))
  names(monthly) <- c("date","num")
  quarterly <- sc2byDate %>% group_by(quarters) %>% summarise(num = sum(num))
  names(quarterly) <- c("date","num")
  
  return(list("weekly"=weekly,"monthly"=monthly,"quarterly"=quarterly))
}

seqFreqPlot <- function(data){
  fig <- plot_ly()
  # total number
  fig <- fig %>% add_trace(
    type = "bar",
    x = data$date,
    y = data$num,
    name = 'Total'
  )
  ## If we want the color of the plot to match uw page background
  # fig <- fig %>% layout(
  #   plot_bgcolor='rgb(247, 247, 247)',
  #   paper_bgcolor='rgb(247, 247, 247)',
  #   fig_bgcolor='rgb(247, 247, 247)'
  # )
  return(fig)
}