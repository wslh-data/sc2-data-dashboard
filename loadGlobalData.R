library(pdftools)

loadGlobalData <- function(rootPath) {
  
  ### Load data
  sc2Data <- do.call(rbind,lapply(list.files(path = rootPath, pattern ='gisaid_hcov-19_.*tsv', full.names = TRUE), read.csv, sep="\t"))
  sc2Data <- unique(sc2Data,by="Accession.ID")
  sc2Data <- sc2Data[!is.na(sc2Data$Accession.ID),]
  # fix 427/429
  sc2Data$Lineage[sc2Data$Lineage=="B.1.427"] <- "B.1.427/429"
  sc2Data$Lineage[sc2Data$Lineage=="B.1.429"] <- "B.1.427/429"
  sc2Data <<- sc2Data
  
  dhsdata <<- read.csv(file.path(rootPath,"County_Table_data.csv"))
  pdf_combine(input = list.files(path = rootPath, pattern ='gisaid_hcov-19_acknowledgement_table.*pdf', full.names = TRUE), output = file.path(rootPath,"gisaid_acknowledgements.pdf"))
  ackfile <<- file.path(rootPath,"gisaid_acknowledgements.pdf")
  
  ### Update Time
  fileName <- str_split(tail(sort(list.files(path = rootPath, pattern ='gisaid_hcov-19_.*tsv')),n=1),"_")[[1]]
  update_time <<- paste(fileName[4],fileName[5],fileName[3],sep="/")
  
  ### GeoJSON Files
  WICounty_geojson <<- fromJSON(file=file.path(rootPath,"geojson-counties-fips.json"))
  herc_geojson <<- fromJSON(file=file.path(rootPath,"Wisconsin_Healthcare_Emergency_Readiness_Coalition_Regions.json"))
  
  ############################
  ##### Pre-analyze Data #####
  ############################
  
  #### Remove last 2 weeks of data from sc2Data
  date_filter <<- as.Date(sc2Data$Collection.date) < (Sys.Date() - 21)
  sc2Data <<- sc2Data[date_filter,]
  
  #### Remove Samples with Blank Lineage
  sc2Data <- sc2Data[sc2Data$Lineage != "",]
  
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
  
  b1429b1427 <<- valueBox(
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.427/429",]),
    subtitle = "B.1.427 & B.1.429 (Epsilon)",
    icon = icon("virus"),
    width = NULL,
    color = "yellow",
    href = "https://outbreak.info/situation-reports"
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