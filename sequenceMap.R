library(plotly)
library(usmap)
library(stringr)
library(viridis)

source("county_to_herc.R")

plotCountyMap <- function(gData,eData){
  # filter out everything but WI
  c = 1
  filteredfeatures <- c()
  for(f in WICounty_geojson$features){
    if(f$properties$STATE == "55"){
      filteredfeatures[[c]] <- f
      c <- c + 1
    }
  }
  WICounty_geojson$features <- filteredfeatures
  #get county
  gData$County <- sapply(gData$Location,function(x) gsub("North America / USA / Wisconsin ?/? ?","",as.character(x)))
  gData$County <- sapply(gData$County,function(x) gsub(" [C,c]ounty","",as.character(x)))
  gData$County <- tolower(gData$County)

  # organize data by county
  countyCounts <- as.data.frame(table(gData$County))
  names(countyCounts) <- c("County","Freq")

  # create empty CountyData dataframe
  c = 1
  CountyData <- c()
  for(i in WICounty_geojson$features){
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
  names(eData) <- c("County","ConfirmedCases")
  eData$County <- tolower(eData$County)
  CountyData <- merge(CountyData,eData,by="County")
  CountyData$percentseq <- (CountyData$Freq / CountyData$ConfirmedCases) * 100
  CountyData$percentseq <- round(CountyData$percentseq,digits = 1)

  # format county names
  CountyData$County <- str_to_title(CountyData$County)

  # gen log for color scale
  CountyData$Log <- log10(CountyData$Freq)
  CountyData$Log[CountyData$Log=="-Inf"] <- 0

  #Hover Format
  CountyData$hover <- with(CountyData, paste(County,"County", '<br>',
                                             "Confirmed Cases:", ConfirmedCases,'<br>',
                                             "Number of Sequences:", Freq,'<br>',
                                             "Percent Cases Sequenced:\t", percentseq,"%"))

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
    geojson=WICounty_geojson,
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

plotHERCMap <- function(data){
  #get county
  data$County <- sapply(data$Location,function(x) gsub("North America / USA / Wisconsin ?/? ?","",as.character(x)))
  data$County <- sapply(data$County,function(x) gsub(" [C,c]ounty","",as.character(x)))
  data <- data[data$County != "",]
  data$County <- tolower(data$County)
  data$HERC <- CountyToHERC(data$County)
  data <- data[! is.na(data$HERC),]

  # organize data by County
  HERCCounts <- as.data.frame(table(data$County), stringsAsFactors=FALSE)
  names(HERCCounts) <- c("HERC","Freq")

  #convert county to HERC and combine
  HERCCounts$HERC <- CountyToHERC(HERCCounts$HERC)
  HERCCounts <- HERCCounts %>% group_by(HERC) %>% summarise(Freq = sum(Freq))

  # create empty HERCData dataframe
  c = 1
  HERCData <- c()
  for(i in HERC_geojson$features){
    HERCData[c] <- i$properties$NAME[]
    c = c + 1
  }
  HERCData <- as.data.frame(HERCData)
  names(HERCData) <- c('HERC')
  HERCData <- merge(HERCData,HERCCounts,by = "HERC", all.x = TRUE)
  HERCData$Freq[is.na(HERCData$Freq)] <- 0

  # add variant counts
  emptyFrame <- data.frame(matrix(ncol=length(WHO_VOC)+2,nrow = nrow(HERCData)))
  colnames(emptyFrame) <- c(WHO_VOC,"VarSum","Total")
  emptyFrame[is.na(emptyFrame)] <- 0
  HERCData <- cbind(HERCData,emptyFrame)
  
  for( i in 1:nrow(data)){
    v <- c(getWHO(data$Lineage[i]),data$HERC[i])
    if(!any(is.na(data))){
      if(v[1] %in% WHO_VOC){
        HERCData[HERCData$HERC==v[2],which(colnames(HERCData)==v[1])] = HERCData[HERCData$HERC==v[2],which(colnames(HERCData)==v[1])] + 1
        HERCData[HERCData$HERC==v[2],which(colnames(HERCData)=='VarSum')] = HERCData[HERCData$HERC==v[2],which(colnames(HERCData)=='VarSum')] + 1
        HERCData[HERCData$HERC==v[2],which(colnames(HERCData)=='Total')] = HERCData[HERCData$HERC==v[2],which(colnames(HERCData)=='Total')] + 1
      }
      else{
        HERCData[HERCData$HERC==v[2],which(colnames(HERCData)=='Total')] = HERCData[HERCData$HERC==v[2],which(colnames(HERCData)=='Total')] + 1
      }
    }
  }
  
  #Build hover template
  for(row in seq(1,length(HERCData$HERC))){
    hover <- c("HERC Region: ",HERCData$HERC[row],'<br>')
    for ( w in WHO_VOC){
      hover <- c(hover,w,': ',HERCData[row,w],'<br>')
    }
    hover <- c(hover,"Variants of Concern:",trunc((HERCData$VarSum[row]/HERCData$Total[row])*100,2),'%<br>')
    hover <- c(hover,"Total Sequences:",HERCData$Total[row],'<br>')
    HERCData[row,'hover'] <- paste(hover,collapse = "")
  }
  
  

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
    geojson=HERC_geojson,
    z = (HERCData$VarSum/HERCData$Total)*100,
    featureidkey="properties.NAME",
    locations = HERCData$HERC,
    text = HERCData$hover,
    hoverinfo = "text",
    showlegend = FALSE,
    color = HERCData$VarSum,
    colors = "Blues",
    colorbar = list(ticksuffix="%",title=list(text="Variants of Concern")),
    marker = list(line = l)
  )
  figureTitle <- paste("\nVariants sequenced by HERC region between",format(min(as.Date(data$DOC)),"%m/%d/%y"),"and", format(max(as.Date(data$DOC)),"%m/%d/%y"))
  fig <- fig %>% layout(title=figureTitle,mapbox=m,margin = list(l=0,r=0,t=75,b=0),autosize=TRUE)
  fig <- fig %>% config(scrollZoom=FALSE,displayModeBar='hover')
  return(fig)
}
