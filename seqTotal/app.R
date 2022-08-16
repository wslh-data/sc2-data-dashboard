#seqTotal

library(shiny)
library(shinycssloaders)
library(plotly)
library(RAthena)
library(lubridate)

# athena connection
athenaConnection <- dbConnect(athena(),
                              s3_staging_dir = "s3://prod-wslh-public-data/sc2dashboard/",
                              work_group = 'prod-sc2dashboard',
                              region_name='us-east-2')

#data fetch and light processing function
getData <- function(){
  data <- dbGetQuery(athenaConnection,"SELECT covv_collection_date,total FROM \"sc2dataportal\".\"prod_gisaid_sars_cov_2_variant_counts\"")
  data <- data[order(covv_collection_date),]
  return(data)
}

ui <- fluidPage(
    fluidRow(
      plotlyOutput(outputId = "totalSeq")%>% withSpinner(color="#c5050c")
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