#seqTotal

library(shiny)
library(shinycssloaders)
library(plotly)
library(RAthena)
library(lubridate)
library(later)

data <- NULL

#data fetch and light processing function
getData <- function(){
  print('Fetching data from AWS')
  print(Sys.time())
  # athena connection
  athenaConnection <- dbConnect(athena(),
                                s3_staging_dir = "s3://prod-wslh-public-d/sc2dashboard/",
                                work_group = 'prod-sc2dashboard',
                                region_name='us-east-2')
  d <- dbGetQuery(athenaConnection,"SELECT covv_collection_date,total FROM \"sc2dataportal\".\"prod_gisaid_sars_cov_2_variant_counts\"")
  dbDisconnect(athenaConnection)
  d <- d[order(covv_collection_date),]
  data <<- d
}

# fetech data from AWS and schedule for update every 6 hours
getData()
later(getData,60*60*6)


ui <- fluidPage(
    fluidRow(
      plotlyOutput(outputId = "totalSeq")%>% withSpinner(color="#c5050c")
    )
)

server <- function(input, output) {

  output$totalSeq <- renderPlotly({
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