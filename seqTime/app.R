#seqTime

library(shiny)
library(shinycssloaders)
library(plotly)
library(noctua)
library(lubridate)

databytime <- NULL

AthenaQueryName <- "sc2_variants_all"

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
  
  d <- d[order(covv_collection_date),]
  d <- within(d, {
    weeks <- paste(epiweek(covv_collection_date),epiyear(covv_collection_date),sep='-')
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(covv_collection_date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(covv_collection_date), format(covv_collection_date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  d <- d[!is.na(d$covv_collection_date),]
  d <- droplevels(d)
  
  weekly <- d %>% group_by(weeks) %>% summarise(total = sum(total))
  names(weekly) <- c("covv_collection_date","total")
  monthly <- d %>% group_by(months) %>% summarise(total = sum(total))
  names(monthly) <- c("covv_collection_date","total")
  quarterly <- d %>% group_by(quarters) %>% summarise(total = sum(total))
  names(quarterly) <- c("covv_collection_date","total")
  
  databytime <<- list("Weekly"=weekly,"Monthly"=monthly,"Quarterly"=quarterly)
}

# interaction components
ui <- fluidPage(
    fluidRow(
      column(width=12,
        plotlyOutput(outputId = "plot")%>% withSpinner(color="#c5050c")
      )
    ),
    fluidRow(
      column(width=4,offset=4,
        selectInput("timeselect", "Time Period:", choices = c("Weekly","Monthly","Quarterly"), selected = "Quarterly")
      )
    )
)


server <- function(input, output) {
  
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))

  output$plot <- renderPlotly({
    
    data <- reactiveGetData()

    fig <- plot_ly()
    # total number
    fig <- fig %>% add_trace(
      type = "bar",
      x = databytime[[input$timeselect]]$covv_collection_date,
      y = databytime[[input$timeselect]]$total,
      name = 'Total'
    )
  })
  
}
shinyApp(ui, server)