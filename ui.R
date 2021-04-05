library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)

sideBarText <- {
  tags$p("The data available in this dashboard was obtained from the",tags$a(href='https://www.gisaid.org/',tags$img(src='https://www.gisaid.org/fileadmin/gisaid/img/schild.png', alt='GISAID',width='50')),"database and the Wisconsin Department of Health Services SARS-CoV-2 dashboard. For additional SARS-CoV-2 information in Wisconsin visit the dashboard here",tags$a(href="https://www.dhs.wisconsin.gov/covid-19/data.htm","COVID-19: Wisconsin Summary Data."),"The data summarized in this report was the result of a combined effort between",tags$a(href="http://www.slh.wisc.edu/","WSLH")," and its academic, clinical, and public health partners including: ",tags$a(href="https://www.dhs.wisconsin.gov/","DHS"),", ",tags$a(href="https://dholk.primate.wisc.edu/wiki/home/page.view?name=home_index","UW-Madison AIDS Vaccine Research Laboratory"),", ",tags$a(href="https://www.gundersenhealth.org/foundation/","Gundersen Medical Foundation"), ", ",tags$a(href="https://city.milwaukee.gov/Health/Services-and-Programs/healthlab","City of Milwaukee Health Department Laboratory"),", and the ",tags$a(href="https://www.cdc.gov/","CDC."))
}

voctext <- HTML('<ul>
  <li> <h3>B.1.1.7</h3> Also known as 20I/501Y.V1 was initially found in December 2020 and first identified in the United Kingdom. This variant carries 17 defining mutations and has spread globally. Early evidence suggests the variant may be associated with increased transmissibility and risk of death. <a href="https://outbreak.info/situation-reports?pango=B.1.1.7">More Information</a>
  </li>
  
  <li><h3>B.1.351</h3> Also known as 20H/501Y.V2 was initially found in December 2020 and first identified in South Africa. This variant carries 9 defining mutations. Preliminary evidence from non-peer reviewed studies suggest this variant could affect vaccine effectiveness. <a href="https://outbreak.info/situation-reports?pango=B.1.351">More Information</a>
  </li>
  
  <li><h3>B.1.429 & B.1.427</h3>Also known as (CAL.20C) and first identified in Southern California in July 2020. Preliminary studies suggest this variant could affect antibody binding. More Information: <a href="https://outbreak.info/situation-reports?pango=B.1.427">B.1.427</a>, <a href="https://outbreak.info/situation-reports?pango=B.1.429">B.1.429</a>
  </li>
  
  <li><h3>P.1</h3>Also known as 20J/501Y.V3 was initially found in January 2021 and first identified in Brazil. This variant carries 16 defining mutations. Some preliminary evidence from non-peer reviewed studies suggest this variant could affect vaccine effectiveness. <a href="https://outbreak.info/situation-reports?pango=P.1">More Information</a>
  </li>
</ul>
For more information on these variants of concern visit <a href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/variant-surveillance/variant-info.html#Concern">CDC\'s Variant Surveillance</a> or Wisconsin DHS <a href="https://www.dhs.wisconsin.gov/covid-19/variants.htm">Emerging SARS-CoV-2 Variants</a>.')

voitext <- HTML('<ul>
  <li><h3>P.2</h3>First identified in Brazil in January 2021. <a href="https://outbreak.info/situation-reports?pango=P.2">More Information</a>
  </li>

  <li><h3>B.1.525</h3>First identified in New York, November 2020. <a href="https://outbreak.info/situation-reports?pango=B.1.525">More Information</a>
  </li>

  <li><h3>B.1.526</h3>First identified in New York, November 2020. <a href="https://outbreak.info/situation-reports?pango=B.1.526">More Information</a>
  </li>

</ul>
For more information on these variants of interest visit <a href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/variant-surveillance/variant-info.html#Interest">CDC\'s Variant Surveillance</a>. ')

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
      sidebarPanel(tags$h2("Synopsis",style="margin-top: 10px;margin-bottom: 10px"),sideBarText,tags$h2("Variant Counts",style="margin-top: 10px;margin-bottom: 10px"),valueBoxOutput("b117vb.a",width=NULL),valueBoxOutput("b1351vb.a",width=NULL),valueBoxOutput("p1vb.a",width=NULL)),
      mainPanel(
        fluidRow(
          tabsetPanel(
            tabPanel("Cumulative Sequences", 
              tags$h3("Cumulative Sequences From Wisconsin"),
              plotlyOutput("totalSequences")%>% withSpinner(color="#c5050c"),
              HTML('Cumulative number of sequences originating from Wisconsin patients in <a href="https://www.gisaid.org/"> <img src="https://www.gisaid.org/fileadmin/gisaid/img/schild.png" alt="GISAID" width="50"/></a> by date of sample collection.')
            ),
            tabPanel("Sequences by Time Period",
              tags$h3("Sequences From Wisconsin by Timeframe"),
              selectInput("timefreqchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly"),
              plotlyOutput("sequenceByTimeframe")%>% withSpinner(color="#c5050c"),
              HTML('Number of sequences originating from Wisconsin patients in <a href="https://www.gisaid.org/"> <img src="https://www.gisaid.org/fileadmin/gisaid/img/schild.png" alt="GISAID" width="50"/></a> by selected time period of sample collection.')
            ),
            tabPanel("Proportion of Lineages",
               tags$h3("Proportion of Lineages by Timeframe"),
               selectInput("timelinchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly"),
               plotlyOutput("lineageByTimeFrame")%>% withSpinner(color="#c5050c"),
               HTML('Proportion of sequence lineages originating from Wisconsin patients in <a href="https://www.gisaid.org/"> <img src="https://www.gisaid.org/fileadmin/gisaid/img/schild.png" alt="GISAID" width="50"/></a> by selected time period of sample collection.')
            )
          )
        )
      )
    ),
    tabPanel("Variant Report",
      sidebarPanel(tags$h2("Synopsis",style="margin-top: 10px;margin-bottom: 10px"),sideBarText,tags$h2("Variant Counts",style="margin-top: 10px;margin-bottom: 10px"),valueBoxOutput("b117vb.b",width=NULL),valueBoxOutput("b1351vb.b",width=NULL),valueBoxOutput("p1vb.b",width=NULL)),
      mainPanel(
       fluidRow(
         tabsetPanel(
           tabPanel("Variants of Concern", 
                    tags$h3("Cumulative number of Variants of Concern"),
                    plotlyOutput("VOC")%>% withSpinner(color="#c5050c"),
                    voctext),
           tabPanel("Variants of Interest", 
                    tags$h3("Cumulative number of Variants of Interest"),
                    plotlyOutput("VOI")%>% withSpinner(color="#c5050c"),
                    voitext),
           tabPanel("Proportion of Variants", 
                  tags$h3("Proportion of Variants Sequenced"),
                  selectInput("timevarchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly"),
                  plotlyOutput("sequenceVariantByTimeframe")%>% withSpinner(color="#c5050c"),
                  HTML('Proportion of Variants originating from Wisconsin patients in <a href="https://www.gisaid.org/"> <img src="https://www.gisaid.org/fileadmin/gisaid/img/schild.png" alt="GISAID" width="50"/></a> by selected time period of sample collection.'))
          )
       )
     )
    ),
    tabPanel("Regional/County Report",
      sidebarPanel(tags$h2("Synopsis",style="margin-top: 10px;margin-bottom: 10px"),sideBarText,tags$h2("Variant Counts",style="margin-top: 10px;margin-bottom: 10px"),valueBoxOutput("b117vb.c",width=NULL),valueBoxOutput("b1351vb.c",width=NULL),valueBoxOutput("p1vb.c",width=NULL)),
      mainPanel(
        fluidRow(
          tabsetPanel(
            tabPanel("Sequences by County", 
                     tags$h3("Sequences by County"),
                     plotlyOutput("countyMap")%>% withSpinner(color="#c5050c"),
                     HTML('Proportion of confirmed cases sequenced from <a href="https://www.gisaid.org/"> <img src="https://www.gisaid.org/fileadmin/gisaid/img/schild.png" alt="GISAID" width="50"/></a> by County, lighter colors represent a larger proportion of cases sequenced. Hover over the map to see what percentage of confirmed cases have been sequenced and the total number of sequences from a particular county. Confirmed cases by county are also available <a href="https://www.dhs.wisconsin.gov/covid-19/county.htm">here</a>.')),
            tabPanel("Variants by HERC Region", 
                     tags$h3("Variants by HERC Region"),
                     plotlyOutput("hercVariant")%>% withSpinner(color="#c5050c"),
                     HTML('Variants of concern in <a href="https://www.gisaid.org/"> <img src="https://www.gisaid.org/fileadmin/gisaid/img/schild.png" alt="GISAID" width="50"/></a> by <a href="https://www.dhs.wisconsin.gov/preparedness/healthcare/index.htm">Healthcare Emergency Readiness Coalition (HERC)</a> region, lighter colors represent a larger number of total variants. Hover over a region to see a breakdown of the variants of concern.'))
          )
        )
      )
    )
  )
)
