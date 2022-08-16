#seqTime

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
  data <- within(data, {
    weeks <- paste(epiweek(covv_collection_date),epiyear(covv_collection_date),sep='-')
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(covv_collection_date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(covv_collection_date), format(covv_collection_date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  data <- data[!is.na(data$covv_collection_date),]
  data <- droplevels(data)
  
  weekly <- data %>% group_by(weeks) %>% summarise(total = sum(total))
  names(weekly) <- c("covv_collection_date","total")
  monthly <- data %>% group_by(months) %>% summarise(total = sum(total))
  names(monthly) <- c("covv_collection_date","total")
  quarterly <- data %>% group_by(quarters) %>% summarise(total = sum(total))
  names(quarterly) <- c("covv_collection_date","total")
  
  return(list("Weekly"=weekly,"Monthly"=monthly,"Quarterly"=quarterly))
}

# interaction components
ui <- fluidPage(
    fluidRow(
      plotlyOutput(outputId = "plot")%>% withSpinner(color="#c5050c")
    ),
    fluidRow(
      selectInput("timeselect", "Time Period:", choices = c("Weekly","Monthly","Quarterly"), selected = "Quarterly")
    )
)


server <- function(input, output) {
  # cache the data processing by day
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  output$plot <- renderPlotly({
    databytime <- reactiveGetData()
    
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