#variant search

library(shiny)
library(shinycssloaders)
library(plotly)
library(noctua)
library(lubridate)
library(dplyr)
library(paws)
library(bit64)

# starting variant selection regular expression
selectionChoices <- NULL
data <- NULL
updateTS <- NULL
#Set the Min date to 2020
minDatasetDate <- as.Date('2020-01-01',"%Y-%m-%d")
#Set the Max date to today (will change later when dataset is loaded)
maxDatasetDate <- as.Date(format(Sys.Date(),"%Y-%m-%d"))

#Set the default selection start date to 6 months ago and selection end date as 2 weeks ago (will change when dataset is loaded)
defaultSelectStart <- floor_date(seq(maxDatasetDate, length = 2, by = "-6 months")[2], unit='week', week_start = 1)
defaultSelectEnd <- floor_date(seq(maxDatasetDate, length = 2, by = "-2 weeks")[2], unit='week', week_start = 1)

AthenaQueryName <- "sc2_variants_all"

#data fetch and light processing function
getData <- function(){
  print('Fetching data from AWS')
  print(Sys.time())
  updateTS <<- format(Sys.time(),"%Y-%m-%d")
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
  
  d <- d[!(is.na(d$covv_lineage) | d$covv_lineage=="" | d$covv_lineage=="Unassigned"), ]
  d <- d %>% mutate(week = floor_date(covv_collection_date, unit = 'week', week_start = 1))
  d <- aggregate(d$total, by=list(week=d$week,lineage=d$covv_lineage),FUN=sum)
  d <- d[order(d$week),]
  colnames(d) <- c('week','lineage','total')
  selectionChoices <<- sort(unique(d$lineage))
  minDatasetDate <<- min(d$week, na.rm = TRUE)
  maxDatasetDate <<- max(d$week, na.rm = TRUE)
  defaultSelectStart <<- floor_date(seq(maxDatasetDate, length = 2, by = "-6 months")[2], unit='week', week_start = 1)
  defaultSelectEnd <<- floor_date(maxDatasetDate, unit='week', week_start = 1)
  data <<- d
}
getData()

ui <- fluidPage(
  fluidRow(
    plotlyOutput(outputId = "totalSeq")%>% withSpinner(color="#c5050c")
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
  ),
  fluidRow(
    column(width=12,
      tags$h4("Variant Selection:"),
      actionButton("showAll","Select All in Date Range",width='300px'),
      actionButton("reset","Reset",width='100px')
    )
  ),
  fluidRow(
    column(width=12,
      selectizeInput("selectVariant",label='',choices=NULL,multiple=TRUE,width='90%')
    )
  ),
  fluidRow(
    column(width=12,
      textOutput("updateTime")
    )
  )
)


server <- function(input, output, session) {
  
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  output$updateTime <- renderText({
    data <- reactiveGetData()
    paste("Last Update: ", as.character(updateTS), ", Latest Available Data Point: ", as.character(maxDatasetDate), sep="")
  })
  
  observe({
    data <- reactiveGetData()
    updateSelectizeInput(session,"selectVariant",choices=selectionChoices,server=TRUE)
  })
  
  observeEvent(input$showAll, {
    sc = sort(unique(data[data$week >= input$dateRange[1] & data$week <= input$dateRange[2],"lineage"]))
    updateSelectizeInput(session,"selectVariant",selected=sc,choices=selectionChoices,server=TRUE)
  })
  
  observeEvent(input$reset,{
    updateSelectizeInput(session,"selectVariant",selected=NULL,choices=selectionChoices,server=TRUE)
  })
  
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
    inc_lineages <- input$selectVariant
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
    plotOtherData <- data[!(data$lineage %in% inc_lineages),]
    if(dim(plotOtherData)[1] != 0){
      plotOtherData <- aggregate(plotOtherData$total,by=list(week=plotOtherData$week),FUN=sum)
      colnames(plotOtherData) <- c('week','total')
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
    }
    
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
        type = "date",
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