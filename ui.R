library(shiny)
library(plotly)

fluidPage(
  tags$link(rel = "stylesheet",type = "text/css", href = "wslh-theme/wslh.css"),
  titlePanel(
    title=tags$div(class="titlePanel",tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='wslh-theme/wslh-logo.png', height = 90))),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "wslh-theme/w-logo.png"), windowTitle="WI SARS-CoV-2 Genomic Report")
  ),
  navbarPage(tags$h4("Wisconsin SARS-CoV-2 Genomic Dashboard"),
             tabPanel("Sequencing Report",
                      sidebarPanel(
                        tags$p("The data available in this dashboard was obtained from the",tags$a(href='https://www.gisaid.org/',tags$img(src='https://www.gisaid.org/fileadmin/gisaid/img/schild.png', alt='GISAID',width='50')),
                               "database and the Wisconsin Department of Health Services SARS-CoV-2 dashboard. For additional SARS-CoV-2 information in 
         Wisconsin visit the dashboard here",tags$a(href="https://www.dhs.wisconsin.gov/covid-19/data.htm","COVID-19: Wisconsin Summary Data"),". The data summarized in this 
         report was the result of a combined effort between",tags$a(href="http://www.slh.wisc.edu/","WSLH")," and its academic, clinical, and public health partners including: 
         ",tags$a(href="https://www.dhs.wisconsin.gov/","DHS"),", ",tags$a(href="https://dholk.primate.wisc.edu/wiki/home/page.view?name=home_index","UW-Madison AIDS Vaccine Research Laboratory"), 
                               ", ",tags$a(href="https://www.gundersenhealth.org/foundation/","Gundersen Medical Foundation"), ", ",tags$a(href="https://city.milwaukee.gov/Health/Services-and-Programs/healthlab","City of Milwaukee Health Department Laboratory"),
                               ", and the ",tags$a(href="https://www.cdc.gov/","CDC"),".")
                        
                      ),
                      mainPanel(
                        fluidRow(
                          tags$h3("Cumulative Sequences From Wisconsin"),
                          column(12,plotlyOutput("totalSequences"))
                        ),
                        fluidRow(
                          tags$h3("Sequences From Wisconsin by Timeframe"),
                          selectInput("timefreqchoice", "Time Period", choices = c("Weekly","Monthly","Quaterly"), selected = "Monthly"),
                          column(12,plotlyOutput("sequenceByTimeframe"))
                        )
                      )),
             tabPanel("Variant Report",
                      sidebarPanel(
               tags$p("The data available in this dashboard was obtained from the",tags$a(href='https://www.gisaid.org/',tags$img(src='https://www.gisaid.org/fileadmin/gisaid/img/schild.png', alt='GISAID',width='50')),
                      "database and the Wisconsin Department of Health Services SARS-CoV-2 dashboard. For additional SARS-CoV-2 information in 
         Wisconsin visit the dashboard here",tags$a(href="https://www.dhs.wisconsin.gov/covid-19/data.htm","COVID-19: Wisconsin Summary Data"),". The data summarized in this 
         report was the result of a combined effort between",tags$a(href="http://www.slh.wisc.edu/","WSLH")," and its academic, clinical, and public health partners including: 
         ",tags$a(href="https://www.dhs.wisconsin.gov/","DHS"),", ",tags$a(href="https://dholk.primate.wisc.edu/wiki/home/page.view?name=home_index","UW-Madison AIDS Vaccine Research Laboratory"), 
                      ", ",tags$a(href="https://www.gundersenhealth.org/foundation/","Gundersen Medical Foundation"), ", ",tags$a(href="https://city.milwaukee.gov/Health/Services-and-Programs/healthlab","City of Milwaukee Health Department Laboratory"),
                      ", and the ",tags$a(href="https://www.cdc.gov/","CDC"),".")
               
             ),
             mainPanel(
               fluidRow(
                 tabsetPanel(
                   tabPanel("Variants of Concern", 
                            tags$h3("WI Variants of Concern"),
                            plotlyOutput("VOC")),
                   tabPanel("Variants of Interest", 
                            tags$h3("WI Variants of Interest"),
                            plotlyOutput("VOI")
                    ),
                   tabPanel("Proportion of Variants", 
                          tags$h3("WI Variants of Interest"),
                          selectInput("timelinchoice", "Time Period", choices = c("Weekly","Monthly","Quaterly"), selected = "Monthly"),
                          plotlyOutput("sequenceLineageByTimeframe")
                    )
                  )
               )
             )
             ),
             tabPanel("Component 3")
  )
)
