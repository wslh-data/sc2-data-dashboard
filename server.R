library(shiny)
library(plotly)
library(rjson)
library(viridis)

source("cumulativeSequences.R")
source("plotVOC.R")
source("plotVOI.R")

### Load data
sc2Data = read.csv(Sys.glob(file.path('./data/gisaid_*tsv')),sep="\t")
dhsdata = read.csv("./data/County_Table_data.csv")

### GeoJSON Files
WICounty_geojson <- fromJSON(file="./data/geojson-counties-fips.json")
herc_geojson <- fromJSON(file="./data/Wisconsin_Healthcare_Emergency_Readiness_Coalition_Regions.json")

############################
##### Pre-analize Data #####
############################

### Sequence Frequencies by Timeframe
sc2byDate <- data.frame(table(sc2Data$Collection.date))
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

sequenceFreqTimeframe <- function(choice){
  if(choice == "Weekly"){
    return(weekly)
  }
  else if(choice == "Quarterly"){
    return(quarterly)
  }
  else{
    return(monthly)
  }
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

#### Sequence Lineage Top List
sc2bylineage <- data.frame(table(sc2Data$Collection.date,sc2Data$Lineage))
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

varWeekly <- group_by_at(sc2bylineage,vars(weeks,lineage)) %>% summarise(num = sum(num))
names(varWeekly) <- c("date","lineage","num")
varMonthly <- group_by_at(sc2bylineage,vars(months,lineage)) %>% summarise(num = sum(num))
names(varMonthly) <- c("date","lineage","num")
varQuarterly <- group_by_at(sc2bylineage,vars(quarters,lineage)) %>% summarise(num = sum(num))
names(varQuarterly) <- c("date","lineage","num")

#keep only top 5 (weekly)
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("date","lineage","num")
colnames(df) <- x
for(week in unique(varWeekly$date)){
  dataHolder <- varWeekly[varWeekly$date == week,]
  dataHolder <- dataHolder[order(dataHolder$num,decreasing = TRUE),]
  a <- dataHolder[1:5,]
  a <- a %>% mutate_at(c("date","lineage"), as.character())
  b <- data.frame("date"=week,"lineage"="other","num"=sum(dataHolder[6:nrow(dataHolder),]$num),stringsAsFactors = FALSE)
  a <- rbind(a,b)
  df <- rbind(df,a)
}
varWeekly <- df

#keep only top 5 (monthly)
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("date","lineage","num")
colnames(df) <- x
for(month in unique(varMonthly$date)){
  dataHolder <- varMonthly[varMonthly$date == month,]
  dataHolder <- dataHolder[order(dataHolder$num,decreasing = TRUE),]
  a <- dataHolder[1:5,]
  a <- a %>% mutate_at(c("date","lineage"), as.character())
  b <- data.frame("date"=month,"lineage"="other","num"=sum(dataHolder[6:nrow(dataHolder),]$num),stringsAsFactors = FALSE)
  a <- rbind(a,b)
  df <- rbind(df,a)
}
varMonthly <- df

#keep only top 5 (quarterly)
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("date","lineage","num")
colnames(df) <- x
for(quarter in unique(varQuarterly$date)){
  dataHolder <- varQuarterly[varQuarterly$date == quarter,]
  dataHolder <- dataHolder[order(dataHolder$num,decreasing = TRUE),]
  a <- dataHolder[1:5,]
  a <- a %>% mutate_at(c("date","lineage"), as.character())
  b <- data.frame("date"=quarter,"lineage"="other","num"=sum(dataHolder[6:nrow(dataHolder),]$num),stringsAsFactors = FALSE)
  a <- rbind(a,b)
  df <- rbind(df,a)
}
varQuarterly <- df

sequenceLineageTimeframe <- function(choice){
  if(choice == "Weekly"){
    return(weekly)
  }
  else if(choice == "Quarterly"){
    return(quarterly)
  }
  else{
    return(monthly)
  }
}

#render a plot of the lineages sequenced by week/month/quarter
plotTimeLineage <- function(data){
  fig <- plot_ly()
  fig <- fig %>% add_trace(
    type = "bar",
    x = data$date,
    y = data$num,
    name = data$lineage,
    color = data$lineage,
    colors = viridis_pal(option = "B")(20),
    hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
    text = data$num
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

### Generate Plots
voiplot <- plotVOI(sc2Data)
vocplot <- plotVOC(sc2Data)
totalseqplot <- cumulativeSequences(sc2Data)


############################
############################
############################



function(input,output,session) { 
  
  ### Total Sequences
  output$totalSequences <- renderPlotly(totalseqplot)
  output$sequenceByTimeframe <- renderPlotly(seqFreqPlot(sequenceFreqTimeframe(input$timefreqchoice)))
  output$sequenceLineageByTimeframe <- renderPlotly(plotTimeLineage(sequenceLineageTimeframe(input$timelinchoice)))
  output$VOC <- renderPlotly(vocplot)
  output$VOI <- renderPlotly(voiplot)

}