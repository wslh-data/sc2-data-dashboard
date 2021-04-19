library(plotly)
library(usmap)
library(stringr)
library(viridis)

source("county_to_herc.R")

plotCountyMap <- function(sc2Data,dhsdata,geojson){
  # filter out everything but WI
  c = 1
  filteredfeatures <- c()
  for(f in geojson$features){
    if(f$properties$STATE == "55"){
      filteredfeatures[[c]] <- f
      c <- c + 1
    }
  }
  geojson$features <- filteredfeatures
  #get county
  sc2Data$County <- sapply(sc2Data$Location,function(x) gsub("North America / USA / Wisconsin ?/? ?","",as.character(x)))
  sc2Data$County <- sapply(sc2Data$County,function(x) gsub(" [C,c]ounty","",as.character(x)))
  sc2Data$County <- tolower(sc2Data$County)

  # organize data by county
  countyCounts <- as.data.frame(table(sc2Data$County))
  names(countyCounts) <- c("County","Freq")

  # create empty CountyData dataframe
  c = 1
  CountyData <- c()
  for(i in geojson$features){
    CountyData[c] <- tolower(i$properties$NAME[])
    c = c + 1
  }
  CountyData <- as.data.frame(CountyData)
  names(CountyData) <- c('County')
  CountyData <- merge(CountyData,countyCounts,by = "County", all.x = TRUE)
  CountyData$Freq[is.na(CountyData$Freq)] <- 0

  # convert counties from names to FIPS code
  CountyData$FIPS <- lapply(CountyData$County, fips, state="WI")

  #load DHS data
  dhsdata <- dhsdata[dhsdata$Measure.Names == "Number of confirmed cases",]
  dhsdata <- dhsdata[,c(1,4)]
  names(dhsdata) <- c("County","ConfirmedCases")
  dhsdata$County <- tolower(dhsdata$County)
  CountyData <- merge(CountyData,dhsdata,by="County")
  CountyData$percentseq <- (CountyData$Freq / CountyData$ConfirmedCases) * 100
  CountyData$percentseq <- round(CountyData$percentseq,digits = 1)




  # add variant counts
  CountyData <- cbind(CountyData,B.1.1.7 = 0,P.1=0,B.1.351=0)

  for( i in 1:nrow(sc2Data)){
    data <- c(as.character(sc2Data[i,15]),sc2Data[i,18])
    if(!any(is.na(data))){
      if(data[1] == "B.1.1.7"){
        CountyData[CountyData$County==data[2],4] = CountyData[CountyData$County==data[2],4] + 1
      }
      if(data[1] == "P.1"){
        CountyData[CountyData$County==data[2],5] = CountyData[CountyData$County==data[2],5] + 1
      }
      if(data[1] == "B.1.351"){
        CountyData[CountyData$County==data[2],6] = CountyData[CountyData$County==data[2],6] + 1
      }
    }
  }

  # format county names
  CountyData$County <- str_to_title(CountyData$County)

  # gen log for color scale
  CountyData$Log <- log10(CountyData$Freq)
  CountyData$Log[CountyData$Log=="-Inf"] <- 0

  #Hover Format
  CountyData$hover <- with(CountyData, paste(County,"County", '<br>',
                                             "Total Sequences:", Freq,'<br>',
                                             "Cases Sequenced:\t", percentseq,"%"))

  # give county boundaries a white border
  l <- list(color = "#CDCDCD", width = 1)

  # specify some map projection/options
  g <- list(
    projection = list(type = 'albers usa'),
    fitbounds = "locations",
    showlakes = TRUE,
    bgcolor = "#fff",
    visible = TRUE
  )
  #mapbox projection options
  m <- list(
    style="white-bg",
    center=list(
      lon=-89.9941,
      lat=44.6243
      ),
    zoom=5,
    bearing=0.8
    )

  fig <- plot_ly()
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=geojson,
    z = CountyData$Log,
    locations = CountyData$FIPS,
    text = CountyData$hover,
    hoverinfo = "text",
    showlegend = FALSE,
    showscale = FALSE,
    color = CountyData$Freq,
    colors = "Purples",
    marker = list(line = l)
  )
  fig <- fig %>% layout(title="\nTotal sequences by County",mapbox=m,margin = list(l=0,r=0,t=75,b=0),autosize=TRUE)
  fig <- fig %>% config(scrollZoom=FALSE)
  return(fig)
}

plotHERCMap <- function(sc2Data,dhsdata,geojson,timerange){
  date_filter <- as.Date(sc2Data$Collection.date) >= min(timerange) & as.Date(sc2Data$Collection.date) <= max(timerange)
  sc2Data <- sc2Data[date_filter,]
  #get county
  sc2Data$County <- sapply(sc2Data$Location,function(x) gsub("North America / USA / Wisconsin ?/? ?","",as.character(x)))
  sc2Data$County <- sapply(sc2Data$County,function(x) gsub(" [C,c]ounty","",as.character(x)))
  sc2Data$County <- tolower(sc2Data$County)
  sc2Data$HERC <- CountyToHERC(sc2Data$County)
  
  # organize data by County
  HERCCounts <- as.data.frame(table(sc2Data$County), stringsAsFactors=FALSE)
  names(HERCCounts) <- c("HERC","Freq")
  
  #convert county to HERC and combine
  HERCCounts$HERC <- CountyToHERC(HERCCounts$HERC)
  HERCCounts <- HERCCounts %>% group_by(HERC) %>% summarise(Freq = sum(Freq))

  # create empty HERCData dataframe
  c = 1
  HERCData <- c()
  for(i in geojson$features){
    HERCData[c] <- i$properties$NAME[]
    c = c + 1
  }
  HERCData <- as.data.frame(HERCData)
  names(HERCData) <- c('HERC')
  HERCData <- merge(HERCData,HERCCounts,by = "HERC", all.x = TRUE)
  HERCData$Freq[is.na(HERCData$Freq)] <- 0
  
  # add variant counts
  HERCData <- cbind(HERCData,B.1.1.7 = 0,P.1=0,B.1.351=0,B.1.427and429=0,Sum=0,Total=0)
  
  for( i in 1:nrow(sc2Data)){
    data <- c(as.character(sc2Data[i,15]),sc2Data[i,19])
    if(!any(is.na(data))){
      if(data[1] == "B.1.1.7"){
        HERCData[HERCData$HERC==data[2],3] = HERCData[HERCData$HERC==data[2],3] + 1
        HERCData[HERCData$HERC==data[2],7] = HERCData[HERCData$HERC==data[2],7] + 1
        HERCData[HERCData$HERC==data[2],8] = HERCData[HERCData$HERC==data[2],8] + 1
      }
      if(data[1] == "P.1"){
        HERCData[HERCData$HERC==data[2],4] = HERCData[HERCData$HERC==data[2],4] + 1
        HERCData[HERCData$HERC==data[2],7] = HERCData[HERCData$HERC==data[2],7] + 1
        HERCData[HERCData$HERC==data[2],8] = HERCData[HERCData$HERC==data[2],8] + 1
      }
      if(data[1] == "B.1.351"){
        HERCData[HERCData$HERC==data[2],5] = HERCData[HERCData$HERC==data[2],5] + 1
        HERCData[HERCData$HERC==data[2],7] = HERCData[HERCData$HERC==data[2],7] + 1
        HERCData[HERCData$HERC==data[2],8] = HERCData[HERCData$HERC==data[2],8] + 1
      }
      if(data[1] == "B.1.429"){
        HERCData[HERCData$HERC==data[2],6] = HERCData[HERCData$HERC==data[2],6] + 1
        HERCData[HERCData$HERC==data[2],7] = HERCData[HERCData$HERC==data[2],7] + 1
        HERCData[HERCData$HERC==data[2],8] = HERCData[HERCData$HERC==data[2],8] + 1
      }
      if(data[1] == "B.1.427"){
        HERCData[HERCData$HERC==data[2],6] = HERCData[HERCData$HERC==data[2],6] + 1
        HERCData[HERCData$HERC==data[2],7] = HERCData[HERCData$HERC==data[2],7] + 1
        HERCData[HERCData$HERC==data[2],8] = HERCData[HERCData$HERC==data[2],8] + 1
      }
      else{
        HERCData[HERCData$HERC==data[2],8] = HERCData[HERCData$HERC==data[2],8] + 1
      }
    }
  }
  
  #Hover Format
  HERCData$hover <- with(HERCData, paste("HERC Region: ",HERC,'<br>',
                                         "B.1.1.7: ",B.1.1.7,'<br>',
                                         "B.1.351:",B.1.351,'<br>',
                                         "B.1.427 / B.1.429:",B.1.427and429,'<br>',
                                         "P.1:",P.1,'<br>',
                                         "Variants Sequenced:",signif((Sum/Total)*100,2),'%<br>',
                                         "Total Sequences:",Total,'<br>'))
                                             
  
  # give county boundaries a white border
  l <- list(color = "#CDCDCD", width = 1)
  
  # specify some map projection/options
  g <- list(
    projection = list(type = 'albers usa'),
    fitbounds = "locations",
    showlakes = TRUE,
    bgcolor = "#fff",
    visible = TRUE
  )
  #mapbox projection options
  m <- list(
    style="white-bg",
    center=list(
      lon=-89.9941,
      lat=44.6243
    ),
    zoom=5,
    bearing=0.8
  )
  
  fig <- plot_ly()
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=geojson,
    z = (HERCData$Sum/HERCData$Total)*100,
    featureidkey="properties.NAME",
    locations = HERCData$HERC,
    text = HERCData$hover,
    hoverinfo = "text",
    showlegend = FALSE,
    color = HERCData$Sum,
    colors = "Blues",
    colorbar = list(ticksuffix="%",title=list(text="Percentage of Variants")),
    marker = list(line = l)
  )
  figureTitle <- paste("\nVariants sequenced by HERC region between",format(min(timerange),"%m/%d/%y"),"and", format(max(timerange),"%m/%d/%y"))
  fig <- fig %>% layout(title=figureTitle,mapbox=m,margin = list(l=0,r=0,t=75,b=0),autosize=TRUE)
  fig <- fig %>% config(scrollZoom=FALSE,displayModeBar='hover')
  return(fig)
}
