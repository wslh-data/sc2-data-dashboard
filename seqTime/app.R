#seqTime

library(shiny)
library(shinycssloaders)
library(plotly)
library(RAthena)
library(lubridate)

databytime <- NULL

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