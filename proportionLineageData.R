library(plotly)
library(dplyr)

prepareLineagePropData <- function(data){
  #### Sequence Lineage Top List
  sc2bylineage <- data.frame(table(data$DOC,data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  sc2bylineage$date <- as.Date(sc2bylineage$date, format= "%Y-%m-%d")
  sc2bylineage <- within(sc2bylineage, {
    weeks <- format(date, "%U-%Y")
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(date), format(date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  sc2bylineage <- sc2bylineage[!is.na(sc2bylineage$date),]
  sc2bylineage <- droplevels(sc2bylineage)
  sc2bylineage <- sc2bylineage %>% mutate_at(c("date","quarters","months","weeks","lineage"), as.character())
  
  varWeekly <- group_by_at(sc2bylineage,vars(weeks,lineage)) %>% summarise(.groups="keep",num = sum(num))
  names(varWeekly) <- c("date","lineage","num")
  varMonthly <- group_by_at(sc2bylineage,vars(months,lineage)) %>% summarise(.groups="keep",num = sum(num))
  names(varMonthly) <- c("date","lineage","num")
  varQuarterly <- group_by_at(sc2bylineage,vars(quarters,lineage)) %>% summarise(.groups="keep",num = sum(num))
  names(varQuarterly) <- c("date","lineage","num")
  
  return(list("weekly"=varWeekly,"monthly"=varMonthly,"quarterly"=varQuarterly))
}

#render a plot of the lineages sequenced by week/month/quarter
plotTimeLineage <- function(data){
  #remove na and none
  data <- data[!is.na(data$lineage),]
  data <- data[data$lineage != "None",]
  fig <- plot_ly()
  #Else
  other_data <- data[!data$lineage %in% VOC_list & !data$lineage %in% VOI_list,]
  fig <- fig %>% add_trace(
    type = "bar",
    x = other_data$date,
    y = other_data$num,
    name = other_data$lineage,
    color = other_data$lineage,
    colors = "Blues",
    hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
    text = other_data$num
  )
  #VOI
  pallet = colorRampPalette(c("#320c55","#c18ff0"))(length(VOI_list))
  c = 1
  for(voi in rev(VOI_list)){
    data_holder <- data[data$lineage %in% VOI_list,]
    fig <- fig %>% add_trace(
      type = "bar",
      x = data_holder$date,
      y = data_holder$num,
      name = voi,
      marker= list(color=pallet[c]),
      hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
      text = data_holder$num
    )
    c = c + 1
  }
  #VOC
  c = 1
  pallet = colorRampPalette(c("#880e0c","#f69593"))(length(VOC_list))
  for(voc in rev(VOC_list)){
    data_holder <- data[data$lineage == voc,]
    fig <- fig %>% add_trace(
      type = "bar",
      x = data_holder$date,
      y = data_holder$num,
      name = voc,
      marker= list(color=pallet[c]),
      hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
      text = data_holder$num
    )
    c = c + 1
  }
  fig <- fig %>% layout(
    barmode="stack",
    barnorm="percent",
    hoverlabel= list(
      font = list(
        size = 14
      )
    ),
    xaxis = list(
      categoryorder = "array",
      categoryarray = data$date
    ),
    yaxis = list(
      categoryorder = "category array",
      categoryarray = data$lineage
    )
  )
  return(fig)
}
