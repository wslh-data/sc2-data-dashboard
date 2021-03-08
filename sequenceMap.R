library(plotly)
library(rjson)
library(usmap)
library(stringr)

renderMap <- function(sc2Data,dhsdata){
  # get geojson data for counties in the US
  geojson <- fromJSON(file="./data/geojson-counties-fips.json")

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
    data <- c(as.character(sc2Data[i,13]),sc2Data[i,15])
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

  # format county names
  CountyData$County <- str_to_title(CountyData$County)

  # gen log for color scale
  CountyData$Log <- log10(CountyData$Freq)
  CountyData$Log[CountyData$Log=="-Inf"] <- 0

  #Hover Format
  CountyData$hover <- with(CountyData, paste(County,"County", '<br>',
                                             "Total Sequences:", Freq,'<br>',
                                             "Percentage of Cases Sequenced:\t", percentseq))

  # give county boundaries a white border
  l <- list(color = "#CDCDCD", width = 1)

  # specify some map projection/options
  g <- list(
    #scope = 'usa',

    projection = list(type = 'albers usa'),
    fitbounds = "locations",
    showlakes = FALSE,
    bgcolor = "#fff",
    visible = FALSE
  )

  fig <- plot_ly()
  fig <- fig %>% add_trace(
    type="choropleth",
    geojson=geojson,
    locationmode = "geojson-id",
    z = CountyData$Log,
    locations = CountyData$FIPS,
    text = CountyData$hover,
    hoverinfo = "text",
    showlegend = FALSE,
    showscale = FALSE,
    color = CountyData$Freq,
    colors = 'Purples',
    marker = list(line = l)
  )
  fig <- fig %>% layout(geo = g,margin = list(l=0,r=0,t=0,b=0),autosize=TRUE)

  return(fig)
}
