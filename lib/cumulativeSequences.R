
#render a plot of the cumulative total of sequences in GISAID
cumulativeSequences <- function(sc2Data){
  # load SC2 data
  sc2byDate <- data.frame(table(sc2Data$DOC))
  names(sc2byDate) <- c("date","num")
  sc2byDate <- sc2byDate[!(sc2byDate$date=="2020"|sc2byDate$date=="2021"),]
  
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