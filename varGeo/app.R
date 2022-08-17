#varGeo

library(shiny)
library(shinycssloaders)
library(leaflet)
library(leaflet.minicharts)
library(RAthena)
library(lubridate)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)

# starting variant selection regular expression
var_expre <- "BA.5.*|BF.*|BE.*|BA.2.12|BA.4.*"
selectionChoices <- NULL

# data fetch and light processing function
getData <- function(){
  # athena connection
  athenaConnection <- dbConnect(athena(),
                                s3_staging_dir = "s3://prod-wslh-public-data/sc2dashboard/",
                                work_group = 'prod-sc2dashboard',
                                region_name='us-east-2')
  data <- dbGetQuery(athenaConnection,"SELECT covv_collection_date,covv_lineage,total,lat,long,county FROM \"sc2dataportal\".\"prod_gisaid_sars_cov_2_variant_counts_county\"")
  dbDisconnect(athenaConnection)
  data <- data[!(is.na(data$covv_lineage) | data$covv_lineage=="" | data$covv_lineage=="Unassigned"), ]
  data <- data[!(is.na(data$lat) | data$lat=="" | is.na(data$long) | data$long==""), ]
  data <- data %>% mutate(week = floor_date(covv_collection_date, unit = 'week', week_start = 1))
  data <- aggregate(data$total, by=list(week=data$week,lineage=data$covv_lineage,lat=data$lat,lng=data$long,county=data$county),FUN=sum)
  data <- data[order(data$week),]
  colnames(data) <- c('week','lineage','lat','lng','county','total')
  selectionChoices <<- sort(unique(data$lineage))
  selectionSelected <- selectionChoices[grep(var_expre,selectionChoices)]
  updateSelectizeInput(getDefaultReactiveDomain(),"selectVariant",selected=selectionSelected,choices=selectionChoices,server=TRUE)
  return(data)
}

# create basemap
us_counties <- readOGR('https://wslhdatacloud.net/assets/geojsons/us-counties-fips.json')
wi_counties <- us_counties[us_counties$STATE=='55',]
bbox <- st_bbox(wi_counties) %>% as.vector()
basemap <- leaflet(wi_counties, width = "100%", height = "650px", options = leafletOptions(zoomControl=FALSE,minZoom=7,maxZoom=7)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-89.9941,lat=44.6243,zoom=7) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  addPolygons(stroke=TRUE,color='black',weight=1,smoothFactor = 0.3,fill=FALSE, fillOpacity = 0.2, fillColor = "#c5050c")

ui <- fluidPage(
  fluidRow(
    leafletOutput(outputId = "map",width="100%",height="650px")%>% withSpinner(color="#c5050c")
  ),
  fluidRow(
    tags$h4("Date Range:"),
    dateRangeInput("dateRange", "",start=(Sys.Date()-51),end=(Sys.Date()-21))
  ),
  fluidRow(
    tags$h4("Variant Selection:"),
    actionButton("showAll","Select All",width='100px'),
    actionButton("reset","Reset",width='100px')
  ),
  fluidRow(
    selectizeInput("selectVariant",label='',choices=NULL,multiple=TRUE,width='100%')
  )
)


server <- function(input, output, session) {
  
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  observeEvent(input$showAll, {
    updateSelectizeInput(session,"selectVariant",selected=selectionChoices,choices=selectionChoices,server=TRUE)
  })
  
  observeEvent(input$reset,{
    selectionSelected <- selectionChoices[grep(var_expre,selectionChoices)]
    updateSelectizeInput(session,"selectVariant",selected=selectionSelected,choices=selectionChoices,server=TRUE)
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    data <- reactiveGetData()
    # aggregate the data
    data <- aggregate(data$total, by=list(lineage=data$lineage,lat=data$lat,lng=data$lng,county=data$county),FUN=sum)
    colnames(data) <- c('lineage','lat','lng','county','total')
    data <- data[order(data$county),]
    chartData <- pivot_wider(data[,c('county','lat','lng','lineage','total')],names_from="lineage",values_from="total")
    
    basemap %>%
      addMinicharts(
        lng = chartData$lng, 
        lat = chartData$lat,
        layerId = chartData$county,
        width = 35, height = 35
      )
  }) 

  observe({
    data <- reactiveGetData()
    # subset by selected date
    data <- data[data$week >= min(input$dateRange) & data$week <= max(input$dateRange),]
    # subset by selected lineages
    if(!is.null(input$selectVariant)){
      data <- data[(data$lineage %in% input$selectVariant),]
    } else {
      selectionSelected <- selectionChoices[grep(var_expre,selectionChoices)]
      data <- data[(data$lineage %in% selectionSelected),]
    }
    
    # aggregate the data
    data <- aggregate(data$total, by=list(lineage=data$lineage,lat=data$lat,lng=data$lng,county=data$county),FUN=sum)
    colnames(data) <- c('lineage','lat','lng','county','total')
    data <- data[order(data$county),]
    chartData <- pivot_wider(data[,c('county','lat','lng','lineage','total')],names_from="lineage",values_from="total")

    chartDataM <- chartData[,-which(names(chartData) %in% c('county','lat','lng'))]
    chartDataM[is.na(chartDataM)] <- 0
    chartDataM <- mutate_all(chartDataM, function(x) as.numeric(as.character(x)))
    chartDataM <- data.matrix(chartDataM)

    leafletProxy("map",session) %>%
      updateMinicharts(
        chartdata = chartDataM,
        layerId = chartData$county
      )
  })
}
shinyApp(ui, server)