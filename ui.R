library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)

sideBarText <- {
  tags$p("The data available in this dashboard was obtained from the",tags$a(href='https://www.gisaid.org/',tags$img(src='https://www.gisaid.org/fileadmin/gisaid/img/schild.png', alt='GISAID',width='50')),"database and the Wisconsin Department of Health Services SARS-CoV-2 dashboard. For additional SARS-CoV-2 information in Wisconsin visit the dashboard here",tags$a(href="https://www.dhs.wisconsin.gov/covid-19/data.htm","COVID-19: Wisconsin Summary Data."),"The data summarized in this report was the result of a combined effort between",tags$a(href="http://www.slh.wisc.edu/","WSLH")," and its academic, clinical, and public health partners including: ",tags$a(href="https://www.dhs.wisconsin.gov/","DHS"),", ",tags$a(href="https://dholk.primate.wisc.edu/wiki/home/page.view?name=home_index","UW-Madison AIDS Vaccine Research Laboratory"),", ",tags$a(href="https://www.gundersenhealth.org/foundation/","Gundersen Medical Foundation"), ", ",tags$a(href="https://city.milwaukee.gov/Health/Services-and-Programs/healthlab","City of Milwaukee Health Department Laboratory"),", and the ",tags$a(href="https://www.cdc.gov/","CDC."))
}

fluidPage(
  tags$style("@import url(https://use.fontawesome.com/releases/v5.15.3/css/all.css);"),
  useShinydashboard(),
  tags$link(rel = "stylesheet",type = "text/css", href = "wslh-theme/wslh.css"),
  titlePanel(
    title=tags$div(class="titlePanel",tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='wslh-theme/wslh-logo.png', height = 90))),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "wslh-theme/w-logo.png"), windowTitle="WI SARS-CoV-2 Genomic Report")
  ),
  navbarPage(tags$h3("Wisconsin SARS-CoV-2 Genomic Dashboard",style="margin:0px"),
    tabPanel("Sequencing Report",
      sidebarPanel(sideBarText,tags$h4("Variant Counts"),valueBoxOutput("b117vb",width=NULL),valueBoxOutput("b1351vb",width=NULL),valueBoxOutput("p1vb",width=NULL)),
      mainPanel(
        fluidRow(
          tabsetPanel(
            tabPanel("Cumulative Sequences", 
              tags$h3("Cumulative Sequences From Wisconsin"),
              plotlyOutput("totalSequences")%>% withSpinner(color="#c5050c")
            ),
            tabPanel("Sequences by Time Period",
              tags$h3("Sequences From Wisconsin by Timeframe"),
              selectInput("timefreqchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly"),
              plotlyOutput("sequenceByTimeframe")%>% withSpinner(color="#c5050c")
            ),
            tabPanel("Proportion of Lineages",
               tags$h3("Proportion of Lineages by Timeframe"),
               selectInput("timelinchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly"),
               plotlyOutput("lineageByTimeFrame")%>% withSpinner(color="#c5050c")
            )
          )
        )
      )
    ),
    tabPanel("Variant Report",
      sidebarPanel(sideBarText),
      mainPanel(
       fluidRow(
         tabsetPanel(
           tabPanel("Variants of Concern", 
                    tags$h3("Cumulative number of Variants of Concern"),
                    plotlyOutput("VOC")%>% withSpinner(color="#c5050c")),
           tabPanel("Variants of Interest", 
                    tags$h3("Cumulative number of Variants of Interest"),
                    plotlyOutput("VOI")%>% withSpinner(color="#c5050c")
            ),
           tabPanel("Proportion of Variants", 
                  tags$h3("Proportion of Variants Sequenced"),
                  selectInput("timevarchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly"),
                  plotlyOutput("sequenceVariantByTimeframe")%>% withSpinner(color="#c5050c")
            )
          )
       )
     )
    ),
    tabPanel("Regional/County Report",
      sidebarPanel(sideBarText),
      mainPanel(
        fluidRow(
          tabsetPanel(
            tabPanel("Sequences by County", 
                     tags$h3("Sequences by County"),
                     plotlyOutput("countyMap")%>% withSpinner(color="#c5050c")),
            tabPanel("Variants by HERC Region", 
                     tags$h3("Variants by Healthcare Emergency Readiness Coalition (HERC) Region"),
                     plotlyOutput("hercVariant")%>% withSpinner(color="#c5050c")
            )
          )
        )
      )
    )
  )
)
