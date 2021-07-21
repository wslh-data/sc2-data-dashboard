library(dplyr)
source("retrieve_data.R")

loadGlobalData <- function(rootPath) {
  
  VOI_list <<- c(
    "B.1.525",
    "B.1.526",
    "B.1.526.1",
    "B.1.617",
    "B.1.617.1",
    "B.1.617.3",
    "P.2",
    "B.1.427/429"
  )
  
  VOC_list <<- c(
    "B.1.1.7",
    "B.1.351",
    "B.1.351.1",
    "B.1.351.2",
    "B.1.351.3",
    "P.1",
    "P.1.1",
    "P.1.2",
    "B.1.617.2"
  )
  
  
  ### Load data
  sc2Data <- get_GISAID_Metadata_data()
  sc2Data <- unique(sc2Data,by="GISAID_ID")
  sc2Data <- sc2Data[!is.na(sc2Data$GISAID_ID),]
  # fix 427/429
  sc2Data$Lineage[sc2Data$Lineage=="B.1.427"] <- "B.1.427/429"
  sc2Data$Lineage[sc2Data$Lineage=="B.1.429"] <- "B.1.427/429"
  sc2Data <<- sc2Data
  
  dhsdata <<- get_DHS_county_data()
  
  ### Acknowledgements
  ackdf <<- sc2Data[,c(1,6,7)]
  colnames(ackdf) <<- c("GISAID_Acc_IDs","Date_of_Submission","Submitting_Lab")
  
  ### GeoJSON Files
  WICounty_geojson <<- fromJSON(file=file.path(rootPath,"geojson-counties-fips.json"))
  herc_geojson <<- fromJSON(file=file.path(rootPath,"Wisconsin_Healthcare_Emergency_Readiness_Coalition_Regions.json"))
  
  ############################
  ##### Pre-analyze Data #####
  ############################
  
  #### Remove last 2 weeks of data from sc2Data
  date_filter <<- as.Date(sc2Data$DOC) < (Sys.Date() - 21)
  sc2Data <<- sc2Data[date_filter,]
  
  #### Remove Samples with Blank Lineage
  sc2Data <<- sc2Data[sc2Data$Lineage != "",]
  
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
    subtitle = "B.1.1.7 (Alpha)",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=B.1.1.7"
  )
  
  b1351 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.351",]),
    subtitle = "B.1.351 (Beta)",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=B.1.351"
  )
  
  p1 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "P.1",]),
    subtitle = "P.1 (Gamma)",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=P.1"
  )
  
  b16172 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.617.2",]),
    subtitle = "B.1.617.2 (Delta)",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports?pango=B.1.617.2"
  )
  
  ############################
  ############################
  ############################
  
}