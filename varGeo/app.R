#varGeo
library(shiny)
library(shinycssloaders)
library(leaflet)
library(leaflet.minicharts)
library(RAthena)
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)
library(geojsonio)
library(htmltools)

# data fetch and light processing function
getData <- function(){
  # athena connection
  athenaConnection <- dbConnect(athena(),
                                s3_staging_dir = "s3://prod-wslh-public-data/sc2dashboard/",
                                work_group = 'prod-sc2dashboard',
                                region_name='us-east-2')
  data <- dbGetQuery(athenaConnection,"SELECT covv_collection_date,variant,total,lat,long,county FROM \"sc2dataportal\".\"prod_gisaid_sars_cov_2_variant_counts_county\"")
  dbDisconnect(athenaConnection)
  data <- data[!(is.na(data$variant) | data$variant=="" | data$variant=="Unassigned"), ]
  data <- data[!(is.na(data$lat) | data$lat=="" | is.na(data$long) | data$long==""), ]
  data <- data %>% mutate(week = floor_date(covv_collection_date, unit = 'week', week_start = 1))
  data <- aggregate(data$total, by=list(week=data$week,lineage=data$variant,lat=data$lat,lng=data$long,county=data$county),FUN=sum)
  data <- data[order(data$week),]
  colnames(data) <- c('week','lineage','lat','lng','county','total')
  return(data)
}

# create map
us_counties <- geojson_read('https://data.slhcddcloud.org/assets/geojsons/us-counties-fips.json',what="sp")
wi_counties <- us_counties[us_counties$STATE=='55',]
wi_counties <- wi_counties[order(wi_counties$NAME),]
bbox <- st_bbox(wi_counties) %>% as.vector()
map <- leaflet(wi_counties, width = "100%", height = "650px", options = leafletOptions(zoomControl=TRUE,minZoom=6,maxZoom=9)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-89.9941,lat=44.6243,zoom=7) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  setMaxBounds(bbox[1] - 1, bbox[2] - 1, bbox[3] + 1, bbox[4] + 1)

ui <- fluidPage(
  fluidRow(
    column(width=12,
      leafletOutput(outputId = "map",width="100%",height="600px")%>% withSpinner(color="#c5050c")
    )
  ),
  fluidRow(
    column(width=12,
      tags$h4("Date Range:"),
      sliderInput(inputId = "dateRange",
                  label = '',
                  width = '100%',
                  min = floor_date(as.Date('2020-01-01',"%Y-%m-%d"), unit='week', week_start = 1),
                  max = floor_date(as.Date(format(Sys.Date(),"%Y-%m-%d")), unit='week', week_start = 1),
                  step=7,
                  value = c(
                    floor_date(seq(as.Date(format(Sys.Date(),"%Y-%m-%d")), length = 2, by = "-6 months")[2], unit='week', week_start = 1),
                    floor_date(seq(as.Date(format(Sys.Date(),"%Y-%m-%d")), length = 2, by = "-2 weeks")[2], unit='week', week_start = 1)
                  )
      )
    )
  )
)


server <- function(input, output, session) {
  
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  # Initialize map
  output$map <- renderLeaflet({
    # get data
    data <- reactiveGetData()
    
    # update slider date range
    updateSliderInput(session, "dateRange", 
                      min = floor_date(as.Date('2020-01-01',"%Y-%m-%d"), unit='week', week_start = 1),
                      max = floor_date(as.Date(format(Sys.Date(),"%Y-%m-%d")), unit='week', week_start = 1),
                      value = c(
                        floor_date(seq(as.Date(format(Sys.Date(),"%Y-%m-%d")), length = 2, by = "-6 months")[2], unit='week', week_start = 1),
                        floor_date(seq(as.Date(format(Sys.Date(),"%Y-%m-%d")), length = 2, by = "-2 weeks")[2], unit='week', week_start = 1)
                      )
    )
    
    # subset by selected date
    data <- data[data$week >= min(input$dateRange) & data$week <= max(input$dateRange),]
    
    # aggregate the data
    data <- aggregate(data$total, by=list(lineage=data$lineage,lat=data$lat,lng=data$lng,county=data$county),FUN=sum)
    colnames(data) <- c('lineage','lat','lng','county','total')
    data$total <- as.numeric(data$total)
    data <- data[order(data$lineage),]
    chartData <- pivot_wider(data[,c('county','lat','lng','lineage','total')],names_from="lineage",values_from="total")
    
    # create table for choropleth
    chartDataC <- chartData[,'county']
    chartDataC$total <- rowSums(chartData[,seq(4,ncol(chartData))],na.rm = TRUE)
    chartDataC[is.na(chartDataC)] <- 0
    wi_counties <- us_counties[us_counties$STATE=='55',]
    wi_counties <- wi_counties[order(wi_counties$NAME),]
    wi_counties@data <- merge(wi_counties@data,chartDataC,by.x="NAME",by.y="county",all.x=TRUE)

    # number of VOCs
    voc_count = length(unique(data$lineage))

    # create count matrix for pie chart
    chartDataM <- chartData[,-which(names(chartData) %in% c('county','lat','lng'))]
    chartDataM[is.na(chartDataM)] <- 0
    chartDataM <- data.matrix(chartDataM)
    
    bins <- c(0,50,100,500,1000,5000,Inf)
    pal <- colorBin(colorRampPalette(c("#f7f7f7","#428BCA"))(length(bins)),domain=wi_counties$total,bins=bins)
    
    mapLabels <- paste(
      "County: ", wi_counties$NAME, "<br/>",
      "Total Sequences: ", wi_counties$total, "<br/>"
    ) %>%
      lapply(htmltools::HTML)

    map <- map %>% 
      addPolygons(
        data = wi_counties,
        label = mapLabels,
        stroke=TRUE,
        color='black',
        weight=1,
        smoothFactor = 0.3,
        fill=TRUE,
        fillOpacity = 1,
        fillColor = ~pal(total)
      )
    
    map <- map %>% addLegend(pal = pal, values = wi_counties$total, opacity = 1, title = "Number of Sequences",
                    position = "bottomright")
  
    map %>%
      addMinicharts(
        type="pie",
        colorPalette = c(hcl.colors(voc_count-1, "viridis"),"#CCCCCC"),
        lng = chartData$lng, 
        lat = chartData$lat,
        chartdata = chartDataM,
        layerId = chartData$county,
        width = 35, 
        height = 35
      )
  })
  
  
}
shinyApp(ui, server)