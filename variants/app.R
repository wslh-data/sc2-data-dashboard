#variants

library(shiny)
library(shinycssloaders)
library(plotly)
library(noctua)
library(paws)
library(lubridate)
library(dplyr)
library(bit64)

# starting data
data <- NULL

#Set the Min date to 2020
minDatasetDate <- as.Date('2020-01-01',"%Y-%m-%d")
#Set the Max date to today (will change later when dataset is loaded)
maxDatasetDate <- as.Date(format(Sys.Date(),"%Y-%m-%d"))

#Set the default selection start date to 6 months ago and selection end date as 2 weeks ago (will change when dataset is loaded)
defaultSelectStart <- floor_date(seq(maxDatasetDate, length = 2, by = "-6 months")[2], unit='week', week_start = 1)
defaultSelectEnd <- floor_date(seq(maxDatasetDate, length = 2, by = "-2 weeks")[2], unit='week', week_start = 1)

AthenaQueryName <- "sc2_voc_bydate"

#data fetch and light processing function
getData <- function(){
  print('Fetching data from AWS')
  print(Sys.time())
  
  # athena connection
  pathena =  paws::athena()
  
  # get the named query
  NamedQueries = lapply(pathena$list_named_queries(WorkGroup = "sc2dashboard")$NamedQueryIds,pathena$get_named_query)
  for (NamedQuery in NamedQueries) {
    if (NamedQuery$NamedQuery$Name == AthenaQueryName)
      query = NamedQuery$NamedQuery$QueryString
  }
  
  # setup athena connection
  athenaConnection <- dbConnect(noctua::athena(), work_group = 'sc2dashboard')
  
  # query data
  d <- dbGetQuery(athenaConnection, query)
  dbDisconnect(athenaConnection)
  
  d$covv_collection_date <- as.Date(d$covv_collection_date)
  
  # parse data
  d <- d[!(is.na(d$variant) | d$variant=="" | d$variant=="Unassigned"), ]
  d <- d %>% mutate(week = floor_date(covv_collection_date, unit = 'week', week_start = 1))
  d <- aggregate(d$total, by=list(week=d$week,lineage=d$variant),FUN=sum)
  d <- d[order(d$week),]
  colnames(d) <- c('week','lineage','total')
  minDatasetDate <<- min(d$week, na.rm = TRUE)
  maxDatasetDate <<- max(d$week, na.rm = TRUE)
  defaultSelectStart <<- floor_date(seq(maxDatasetDate, length = 2, by = "-6 months")[2], unit='week', week_start = 1)
  defaultSelectEnd <<- floor_date(maxDatasetDate, unit='week', week_start = 1)
  data <<- d
}
getData()

ui <- fluidPage(
  fluidRow(
    column(width=12,
      plotlyOutput(outputId = "totalSeq")%>% withSpinner(color="#c5050c")
    )
  ),
  fluidRow(
    column(width=12,
      tags$h4("Date Range:"),
      sliderInput(inputId = "dateRange",
        label = '',
        width = '90%',
        min = floor_date(as.Date('2020-01-01',"%Y-%m-%d"), unit='week', week_start = 1),
        max = floor_date(maxDatasetDate, unit='week', week_start = 1),
        step=7,
        value = c(defaultSelectStart, defaultSelectEnd)
      )
    )
  )
)


server <- function(input, output, session) {
  
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  # update slider date range
  updateSliderInput(session, "dateRange", 
                    min = floor_date(as.Date('2020-01-01',"%Y-%m-%d"), unit='week', week_start = 1),
                    max = floor_date(maxDatasetDate, unit='week', week_start = 1),
                    value = c(defaultSelectStart, defaultSelectEnd)
  )
  
  output$totalSeq <- renderPlotly({
    # get data
    data <- reactiveGetData()
    
    # add bar for each selected lineage
    fig <- plot_ly()
    inc_lineages <- unique(data$lineage)
    inc_lineages <- inc_lineages[! inc_lineages == "Other"]
    pallet = colorRampPalette(c("#B0090F","#1F77B4"))(length(inc_lineages))
    counter = 1
    for(lineage in inc_lineages){
      plotData <- data[data$lineage == lineage,]
      fig <- fig %>% add_trace(
        type = "bar",
        x = plotData$week,
        y = plotData$total,
        name = lineage,
        marker= list(color=pallet[counter]),
        hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
        text = plotData$total,
        textposition = "none"
      )
      counter = counter + 1
    }
    
    # add bar for unselected lineages and label as other
    plotOtherData <- data[data$lineage == "Other",]
    # add other bars
    fig <- fig %>% add_trace(
      type = "bar",
      x = plotOtherData$week,
      y = plotOtherData$total,
      name = "Other",
      marker= list(color="#CCCCCC"),
      hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
      text = plotOtherData$total,
      textposition = "none"
    )
    
    options(warn=-1)
    fig <- fig %>% layout(
      barmode="stack",
      barnorm="percent",
      hoverlabel= list(
        font = list(
          size = 14
        )
      ),
      xaxis = list(
        title = "collection date, week starting",
        autotick = FALSE,
        tickmode = "array",
        tickvals = data$week,
        range = c(input$dateRange[1]-weeks(1), input$dateRange[2]+weeks(1)),
        tickformat = "%Y-%m-%d",
        tickangle=90
      ),
      yaxis = list(
        title = "% of viral lineages among sequences",
        categoryorder = "category array",
        categoryarray = data$lineage
      )
    )
    fig
  })
  
}
shinyApp(ui, server)