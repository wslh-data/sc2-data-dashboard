############################
##### Pre-analyze Data #####
############################

preProcess <- function() {
  
  sc2Data <<- sc2Data
  
  ### Generate Acknowledgement Data
  ackdf <<- sc2Data[,c(1,6,7)]
  colnames(ackdf) <<- c("GISAID_Acc_IDs","Date_of_Submission","Submitting_Lab")
  
  #### Remove last 2 weeks of data from sc2Data
  #date_filter <<- as.Date(sc2Data$DOC) < (Sys.Date() - 21)
  #sc2Data <<- sc2Data[date_filter,]
  
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
}

subsetDataByTime <- function(data,start=NULL,end=NULL,timerange=NULL,months_ago=NULL) {
  if(! is.null(timerange)){
    date_filter <- as.Date(data$DOC) >= min(timerange) & as.Date(data$DOC) <= max(timerange)
    data <- data[date_filter,]
  } else if(! is.null(start) & ! is.null(end)){
    date_filter <- as.Date(data$DOC) >= start & as.Date(data$DOC) <= end
    data <- data[date_filter,]
  } else if(! is.null(months_ago)){
    dates <- seq(as.Date(lastUpdate),length =4, by ="-6 months")[-1]
    date <- dates[as.numeric(months_ago)]
    date_filter <- as.Date(data$DOC) >= min(date)
    data <- data[date_filter,]
  } else {
    return(data)
  }
  return(data)
}