#seqTotal

library(shiny)
library(shinycssloaders)
library(plotly)
library(noctua)
library(lubridate)

data <- NULL

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
  data <<- d
}


ui <- fluidPage(
    fluidRow(
      column(width=12,
        plotlyOutput(outputId = "totalSeq")%>% withSpinner(color="#c5050c")
      )
    )
)

server <- function(input, output) {
  
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))

  output$totalSeq <- renderPlotly({
    data <- reactiveGetData()
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$covv_collection_date, format= "%Y-%m-%d"),
      y = cumsum(data$total),
      name = 'Total',
      mode = "lines"
    )
    fig <- fig %>%
      layout(
        xaxis = list(
          type = "date",
          range=c(as.Date("2020-01-31"),as.Date(format(Sys.Date(),"%Y-%m-%d")))
        ),
        hovermode = 'compare',
        autosize=TRUE
      )
    fig
  })
  
}
shinyApp(ui, server)