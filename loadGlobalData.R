library(pdftools)

loadGlobalData <- function(rootPath) {
  
  ### Update Time
  fileName <- str_split(Sys.glob(file.path(rootPath,'gisaid_hcov-19_2021*.tsv')),"_")[[1]]
  update_time <<- paste(fileName[4],fileName[5],fileName[3],sep="/")
  
  ### Load data
  sc2Data_2020 <<- read.csv(Sys.glob(file.path(rootPath,'gisaid_hcov-19_2020*.tsv')),sep="\t")
  sc2Data_2021 <<- read.csv(Sys.glob(file.path(rootPath,'gisaid_hcov-19_2021*.tsv')),sep="\t")
  sc2Data <<- merge(sc2Data_2020,sc2Data_2021,all=TRUE)
  
  dhsdata <<- read.csv(file.path(rootPath,"County_Table_data.csv"))
  ack_2020 <<- Sys.glob(file.path(rootPath,"gisaid_hcov-19_acknowledgement_table_2020*.pdf"))
  ack_2021 <<- Sys.glob(file.path(rootPath,"gisaid_hcov-19_acknowledgement_table_2021*.pdf"))
  pdf_combine(c(ack_2020, ack_2021), output = file.path(rootPath,"gisaid_acknowledgements.pdf"))
  ackfile <<- file.path(rootPath,"gisaid_acknowledgements.pdf")
  
  ### GeoJSON Files
  WICounty_geojson <<- fromJSON(file=file.path(rootPath,"geojson-counties-fips.json"))
  herc_geojson <<- fromJSON(file=file.path(rootPath,"Wisconsin_Healthcare_Emergency_Readiness_Coalition_Regions.json"))
  
  ############################
  ##### Pre-analyze Data #####
  ############################
  
  #### Remove last 2 weeks of data from sc2Data
  date_filter <<- as.Date(sc2Data$Collection.date) < (Sys.Date() - 21)
  sc2Data <<- sc2Data[date_filter,]
  
  #### Sequence Frequency by Time Period
  timeFrameData <<- prepareTimeFrameData(sc2Data)
  
  sequenceFreqTimeframe <<- function(choice){
    if(choice == "Weekly"){
      return(timeFrameData$weekly)
    }
    else if(choice == "Quarterly"){
      return(timeFrameData$quarterly)
    }
    else{
      return(timeFrameData$monthly)
    }
  }
  
  #### Proportion of Lineages Sequenced by Time Period
  
  lineagePropData <<- prepareLineagePropData(sc2Data)
  
  sequenceLineageTimeframe <<- function(choice){
    if(choice == "Weekly"){
      return(lineagePropData$weekly)
    }
    else if(choice == "Monthly"){
      return(lineagePropData$monthly)
    }
    else{
      return(lineagePropData$quarterly)
    }
  }
  
  #### Proportion of Variants Sequenced by Time Period
  
  variantPropData <<- prepareVariantPropData(sc2Data)
  
  sequenceVariantTimeframe <<- function(choice){
    if(choice == "Weekly"){
      return(variantPropData$weekly)
    }
    else if(choice == "Quarterly"){
      return(variantPropData$quarterly)
    }
    else{
      return(variantPropData$monthly)
    }
  }
  
  #### Generate Plots
  voiplot <<- plotVOI(sc2Data)
  vocplot <<- plotVOC(sc2Data)
  totalseqplot <<- cumulativeSequences(sc2Data)
  countyMapPlot <<- plotCountyMap(sc2Data,dhsdata,WICounty_geojson)
  
  
  
  #### Variant Value Box
  b117 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.1.7",]),
    subtitle = "B.1.1.7",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=B.1.1.7"
  )
  
  b1351 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.351",]),
    subtitle = "B.1.351",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=B.1.351"
  )
  
  p1 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "P.1",]),
    subtitle = "P.1",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=P.1"
  )
  
  b1429b1427 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.427"|sc2Data$Lineage == "B.1.429",]),
    subtitle = "B.1.427 & B.1.429",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports"
  )
  
  ############################
  ############################
  ############################
  
}