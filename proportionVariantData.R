library(plotly)
library(dplyr)
library(viridis)

variantList <- c(
  "B.1.1.7",
  "B.1.351",
  "P.1",
  "B.1.429",
  "B.1.427",
  "P.2",
  "B.1.525",
  "B.1.526"
)

prepareVariantPropData <- function(data){
  #### Sequence Lineage Top List
  sc2bylineage <- data.frame(table(data$Collection.date,data$Lineage))
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
  
  weeklyVariantProp <- varWeekly[varWeekly$lineage %in% variantList,]
  sumNonVarData <- varWeekly[!varWeekly$lineage %in% variantList,c(1,3)] %>% summarise(num=sum(num))
  for(week in unique(sumNonVarData$date)){
    if(week %in% weeklyVariantProp$date){
      row <- data.frame(date = sumNonVarData[sumNonVarData$date == week,1],lineage="Other",num = sumNonVarData[sumNonVarData$date == week,2])
      weeklyVariantProp <- bind_rows(weeklyVariantProp,row)
    }
  }
  
  monthlyVariantProp <- varMonthly[varMonthly$lineage %in% variantList,]
  sumNonVarData <- varMonthly[!varMonthly$lineage %in% variantList,c(1,3)] %>% summarise(num=sum(num))
  for(month in unique(sumNonVarData$date)){
    if(month %in% monthlyVariantProp$date){
      row <- data.frame(date = sumNonVarData[sumNonVarData$date == month,1],lineage="Other",num = sumNonVarData[sumNonVarData$date == month,2])
      monthlyVariantProp <- bind_rows(monthlyVariantProp,row)
    }
  }
  
  quarterlyVariantProp <- varQuarterly[varQuarterly$lineage %in% variantList,]
  sumNonVarData <- varQuarterly[!varQuarterly$lineage %in% variantList,c(1,3)] %>% summarise(num=sum(num))
  for(quarter in unique(sumNonVarData$date)){
    if(quarter %in% quarterlyVariantProp$date){
      row <- data.frame(date = sumNonVarData[sumNonVarData$date == quarter,1],lineage="Other",num = sumNonVarData[sumNonVarData$date == quarter,2])
      quarterlyVariantProp <- bind_rows(quarterlyVariantProp,row)
    }
  }
  
  return(list("weekly"=weeklyVariantProp,"monthly"=monthlyVariantProp,"quarterly"=quarterlyVariantProp))
}

#render a plot of the lineages sequenced by week/month/quarter
plotVariantTimeLineage <- function(data){
  data_other <- data[data$lineage =="Other",]
  data_voc <- data[data$lineage =="P.1" | data$lineage =="B.1.351" | data$lineage =="B.1.1.7" | data$lineage =="B.1.429" | data$lineage =="B.1.427",]
  data_voi <- data[data$lineage =="P.2" | data$lineage =="B.1.526" | data$lineage =="B.1.525",]
  data <- data[data$lineage !="Other",]
  fig <- plot_ly()
  fig <- fig %>% add_trace(
    type = "bar",
    x = data_voc$date,
    y = data_voc$num,
    name = data_voc$lineage,
    marker= list(color="#F1605D"),
    hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
    text = data_voc$num
  )
  fig <- fig %>% add_trace(
    type = "bar",
    x = data_voi$date,
    y = data_voi$num,
    name = data_voi$lineage,
    marker= list(color="#451077"),
    hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
    text = data_voi$num
  )
  fig <- fig %>% add_trace(
    type = "bar",
    x = data_other$date,
    y = data_other$num,
    name = data_other$lineage,
    marker= list(color="#CCCCCC"),
    hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
    text = data_other$num
  )
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
