#variants

library(shiny)
library(shinycssloaders)
library(plotly)
library(RAthena)
library(lubridate)
library(dplyr)

# starting variant selection regular expression
selectionChoices <- NULL
data <- NULL
updateTS <- NULL
latestDataPoint <- NULL

#data fetch and light processing function
getData <- function(){
  print('Fetching data from AWS')
  print(Sys.time())
  updateTS <<- format(Sys.time(),"%Y-%m-%d")
  # athena connection
  athenaConnection <- dbConnect(athena(),
                                s3_staging_dir = "s3://prod-wslh-public-data/sc2dashboard/",
                                work_group = 'prod-sc2dashboard',
                                region_name='us-east-2')
  d <- dbGetQuery(athenaConnection,"SELECT covv_collection_date,covv_lineage,total FROM \"sc2dataportal\".\"prod_gisaid_sars_cov_2_variant_counts\"")
  dbDisconnect(athenaConnection)
  d <- d[!(is.na(d$covv_lineage) | d$covv_lineage=="" | d$covv_lineage=="Unassigned"), ]
  latestDataPoint <<- as.character(max(d$covv_collection_date))
  d <- d %>% mutate(week = floor_date(covv_collection_date, unit = 'week', week_start = 1))
  d <- aggregate(d$total, by=list(week=d$week,lineage=d$covv_lineage),FUN=sum)
  d <- d[order(d$week),]
  colnames(d) <- c('week','lineage','total')
  selectionChoices <<- sort(unique(d$lineage))
  data <<- d
}

ui <- fluidPage(
  fluidRow(
    plotlyOutput(outputId = "totalSeq")%>% withSpinner(color="#c5050c")
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
  ),
  fluidRow(
    column(width=12,
      tags$h4("Variant Selection:"),
      actionButton("showAll","Select All",width='100px'),
      actionButton("reset","Reset",width='100px')
    )
  ),
  fluidRow(
    column(width=12,
      selectizeInput("selectVariant",label='',choices=NULL,multiple=TRUE,width='100%')
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
    paste("Last Update: ", as.character(updateTS), ", Latest Available Data Point: ", latestDataPoint, sep="")
  })
  
  observe({
    data <- reactiveGetData()
    updateSelectizeInput(session,"selectVariant",choices=selectionChoices,server=TRUE)
  })
  
  observeEvent(input$showAll, {
    updateSelectizeInput(session,"selectVariant",selected=selectionChoices,choices=selectionChoices,server=TRUE)
  })
  
  observeEvent(input$reset,{
    updateSelectizeInput(session,"selectVariant",selected=NULL,choices=selectionChoices,server=TRUE)
  })
  
  # update slider date range
  updateSliderInput(session, "dateRange", 
                    min = floor_date(as.Date('2020-01-01',"%Y-%m-%d"), unit='week', week_start = 1),
                    max = floor_date(as.Date(format(Sys.Date(),"%Y-%m-%d")), unit='week', week_start = 1),
                    value = c(
                      floor_date(seq(as.Date(format(Sys.Date(),"%Y-%m-%d")), length = 2, by = "-6 months")[2], unit='week', week_start = 1),
                      floor_date(seq(as.Date(format(Sys.Date(),"%Y-%m-%d")), length = 2, by = "-2 weeks")[2], unit='week', week_start = 1)
                    )
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
        title = "collection date, week starting",
        autotick = FALSE,
        tickmode = "array",
        tickvals = data$week,
        range = input$dateRange,
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