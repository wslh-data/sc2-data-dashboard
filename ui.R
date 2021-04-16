library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)

sideBarText <- HTML("<p>The data in this dashboard is obtained from the <a href='https://www.gisaid.org/'><img src='https://www.gisaid.org/fileadmin/gisaid/img/schild.png' alt='GISAID' style='width:50px'></a> database and the Wisconsin Department of Health Services (DHS) <a href='https://www.dhs.wisconsin.gov/covid-19/data.htm'>SARS-CoV-2 dashboard</a>. It includes results generated for Wisconsin residents by WSLH and other labs. Note: Sequencing data may not match the DHS website due to different update frequencies and data sources.</p>")

aboutthedataText <- HTML("<h2 style='margin-top: 10px;margin-bottom: 10px'>About the data</h2><p>The sequencing data results in this dashboard are obtained from the <a href='https://www.gisaid.org/'><img src='https://www.gisaid.org/fileadmin/gisaid/img/schild.png' alt='GISAID' style='width:50px'></a> database and the COVID-19 case numbers from the Wisconsin Department of Health Services (DHS) <a href='https://www.dhs.wisconsin.gov/covid-19/data.htm'>SARS-CoV-2 dashboard</a>.</p><p>Only residual positive viral transport media with sufficient viral load from molecular tests can produce reliable sequence data. All sequences are from samples collected from Wisconsin residents.</p><p>Data is updated daily at 3 pm. Stored samples may be sequenced at a later date altering historic data. On average, sequence data is uploaded about 2-3 weeks after collection from a patient but this varies between laboratories.</p><p>Variant nomenclature is dynamic and can change as new strains are characterized.</p><p>The data summarized in this report was the result of a combined effort between <a href='http://www.slh.wisc.edu/'>WSLH</a> and its academic, clinical, and public health partners including: <a href='https://www.dhs.wisconsin.gov/'>DHS</a>, <a href='https://dholk.primate.wisc.edu/wiki/home/page.view?name=home_index'>UW-Madison AIDS Vaccine Research Laboratory</a>, <a href='https://www.gundersenhealth.org/foundation/'>Gundersen Medical Foundation</a>, <a href='https://city.milwaukee.gov/Health/Services-and-Programs/healthlab'>City of Milwaukee Health Department Laboratory</a>, and the <a href='https://www.cdc.gov/'>CDC</a>. A full list of the laboratories contributing to this data is available here:</p>")

voctext <- HTML('<h3>Cumulative number of variants sequences identified over time by sample collection date.</h3>
<p>Variants of concern have evidence of an increase in transmissibility, more severe disease (increased hospitalizations or deaths), significant reduction in neutralization by antibodies generated during previous infection or vaccination, reduced effectiveness of treatments or vaccines, or diagnostic detection failures. <a href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/variant-surveillance/variant-info.html">More information</a></p>
<ul>
  <li><b>B.1.1.7:</b> Also known as 20I/501Y.V1 was initially found in December 2020 and first identified in the United Kingdom. This variant carries 17 defining mutations and has spread globally. Early evidence suggests the variant may be associated with increased transmissibility and risk of death. <a href="https://outbreak.info/situation-reports?pango=B.1.1.7">More Information</a>
  </li>
  
  <li><b>B.1.351:</b> Also known as 20H/501Y.V2 was initially found in December 2020 and first identified in South Africa. This variant carries 9 defining mutations. Preliminary evidence from non-peer reviewed studies suggest this variant could affect vaccine effectiveness. <a href="https://outbreak.info/situation-reports?pango=B.1.351">More Information</a>
  </li>
  
  <li><b>B.1.429 & B.1.427:</b> Also known as (CAL.20C) and first identified in Southern California in July 2020. Preliminary studies suggest this variant could affect antibody binding. More Information: <a href="https://outbreak.info/situation-reports?pango=B.1.427">B.1.427</a>, <a href="https://outbreak.info/situation-reports?pango=B.1.429">B.1.429</a>
  </li>
  
  <li><b>P.1:</b> Also known as 20J/501Y.V3 was initially found in January 2021 and first identified in Brazil. This variant carries 16 defining mutations. Some preliminary evidence from non-peer reviewed studies suggest this variant could affect vaccine effectiveness. <a href="https://outbreak.info/situation-reports?pango=P.1">More Information</a>
  </li>
</ul>
For more information on these variants of concern visit <a href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/variant-surveillance/variant-info.html#Concern">CDC\'s Variant Surveillance</a> or Wisconsin DHS <a href="https://www.dhs.wisconsin.gov/covid-19/variants.htm">Emerging SARS-CoV-2 Variants</a>.')

voitext <- HTML('<h3>Cumulative number of variants sequences identified over time by sample collection date.</h3>
<p>Variants of Interest contain specific mutations that have been associated with changes to receptor binding, reduced neutralization by antibodies generated against previous infection or vaccination, reduced efficacy of treatments, potential diagnostic impact, or predicted increase in transmissibility or disease severity. <a href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/variant-surveillance/variant-info.html">More information</a></p>
<ul>
  <li><b>P.2:</b> First identified in Brazil in January 2021. <a href="https://outbreak.info/situation-reports?pango=P.2">More Information</a>
  </li>

  <li><b>B.1.525:</b> First identified in New York, November 2020. <a href="https://outbreak.info/situation-reports?pango=B.1.525">More Information</a>
  </li>

  <li><b>B.1.526:</b> First identified in New York, November 2020. <a href="https://outbreak.info/situation-reports?pango=B.1.526">More Information</a>
  </li>

</ul>
For more information on these variants of interest visit <a href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/variant-surveillance/variant-info.html#Interest">CDC\'s Variant Surveillance</a>. ')

fluidPage(
  tags$style("@import url(https://use.fontawesome.com/releases/v5.15.3/css/all.css);"),
  useShinydashboard(),
  tags$link(rel = "stylesheet",type = "text/css", href = "wslh-theme/wslh.css"),
  titlePanel(
    title=tags$div(class="titlePanel",tags$a(href='https://dataportal-test.slh.wisc.edu/',tags$img(src='wslh-theme/wslh-logo.png', height = 90))),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "wslh-theme/w-logo.png"), windowTitle="WI SARS-CoV-2 Genomic Report")
  ),
  navbarPage(id='navtabs',tags$h3("Wisconsin SARS-CoV-2 Genomic Dashboard",style="margin:0px"),
    tabPanel("Sequencing Report",
      sidebarPanel(
        valueBoxOutput("b117vb.a",width=NULL),
        valueBoxOutput("b1351vb.a",width=NULL),
        valueBoxOutput("p1vb.a",width=NULL),
        sideBarText
      ),
      mainPanel(
        fluidRow(
          tabsetPanel(
            tabPanel("Sequences by Time Period",
              plotlyOutput("sequenceByTimeframe")%>% withSpinner(color="#c5050c"),
              tags$h3('Number of sequences by selected time period of sample collection.'),
              selectInput("timefreqchoice", "Time Period:", choices = c("Weekly","Monthly","Quarterly"), selected = "Quarterly")
            ),
            tabPanel("Cumulative Sequences", 
              plotlyOutput("totalSequences")%>% withSpinner(color="#c5050c"),
              tags$h3('Cumulative number of sequences by date of sample collection.')
            ),
            tabPanel("Proportion of Lineages",
               plotlyOutput("lineageByTimeFrame")%>% withSpinner(color="#c5050c"),
               tags$h3('Proportion of all sequence lineages over time by sample collection date.'),
               selectInput("timelinchoice", "Time Period:", choices = c("Weekly","Monthly","Quarterly"), selected = "Monthly")
            )
          )
        )
      )
    ),
    tabPanel("Variant Report",
      sidebarPanel(
        valueBoxOutput("b117vb.b",width=NULL),
        valueBoxOutput("b1351vb.b",width=NULL),
        valueBoxOutput("p1vb.b",width=NULL),
        sideBarText
      ),
      mainPanel(
       fluidRow(
         tabsetPanel(
           tabPanel("Proportion of Variants",
                    plotlyOutput("sequenceVariantByTimeframe")%>% withSpinner(color="#c5050c"),
                    tags$h3("Proportion of sequenced strains that are variants, over time by sample collection date."),
                    selectInput("timevarchoice", "Time Period", choices = c("Weekly","Monthly","Quarterly"), selected = "Weekly")
           ),
           tabPanel("Variants of Concern", 
                    plotlyOutput("VOC")%>% withSpinner(color="#c5050c"),
                    voctext),
           tabPanel("Variants of Interest", 
                    plotlyOutput("VOI")%>% withSpinner(color="#c5050c"),
                    voitext)
          )
       )
     )
    ),
    tabPanel("Regional/County Report",
    sidebarPanel(
      valueBoxOutput("b117vb.c",width=NULL),
      valueBoxOutput("b1351vb.c",width=NULL),
      valueBoxOutput("p1vb.c",width=NULL),
      sideBarText
    ),
      mainPanel(
        fluidRow(
          tabsetPanel(
            tabPanel("Sequences by County",
                     plotlyOutput("countyMap")%>% withSpinner(color="#c5050c"),
                     HTML('Number of confirmed cases sequenced by county, darker colors represent a larger proportion of cases sequenced. Hover over the map to see what percentage of confirmed cases have been sequenced and the total number of sequences from a particular county. Confirmed cases by county are also available <a href="https://www.dhs.wisconsin.gov/covid-19/county.htm">here</a>.')),
            tabPanel("Variants by HERC Region",
                     plotlyOutput("hercVariant")%>% withSpinner(color="#c5050c"),
                     HTML('Variants of concern by <a href="https://www.dhs.wisconsin.gov/preparedness/healthcare/index.htm">Healthcare Emergency Readiness Coalition (HERC)</a> region, darker colors represent a larger number of total variants identified. Hover over a region to see a breakdown of the variants of concern.'))
          )
        )
      )
    ),
    tabPanel("About the Data",aboutthedataText,downloadLink("downloadAck", "Download GISAID Acknowledgements")
    )
  )
)
