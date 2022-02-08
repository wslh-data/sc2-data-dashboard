
#render a plot of the variants
plotCumulativeVariants <- function(sc2Data,variant_list){

  sc2bylineage <- data.frame(table(sc2Data$DOC,sc2Data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  
  lineage_order <- sc2bylineage[,c("lineage","num")]
  lineage_order <- group_by_at(sc2bylineage,vars(lineage)) %>% summarise(.groups="keep",num = sum(num))
  lineage_order <- lineage_order[lineage_order$lineage%in%variant_list,]
  lineage_order <- lineage_order[order(-lineage_order$num),]
  
  fig <- plot_ly()
  for(variant in lineage_order$lineage){
    data <- data.frame(date="2020-01-01",lineage=variant,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$lineage == variant,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = variant,
      mode = "lines"
    )
  }
  
  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      )
    )
  fig
  return(fig)
}