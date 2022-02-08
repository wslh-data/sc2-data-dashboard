
### Function to pre-analyze data to be subsetable by timescale and pango vs who
prepareVariantPropData <- function(data){
  variantList <- unlist(WHO_list)
  sc2bylineage <- data.frame(table(data$DOC,data$Lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage$who <- unlist(lapply(sc2bylineage$lineage,getWHO))
  sc2bylineage <- sc2bylineage[which(!(sc2bylineage$date=="2020" | sc2bylineage$date=="2021" | sc2bylineage$date=="2022")),]
  sc2bylineage$date <- as.Date(sc2bylineage$date, format= "%Y-%m-%d")
  sc2bylineage <- within(sc2bylineage, {
    weeks <- paste(epiweek(date),epiyear(date),sep='-')
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(date), format(date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  sc2bylineage <- sc2bylineage[!is.na(sc2bylineage$date),]
  sc2bylineage <- droplevels(sc2bylineage)
  sc2bylineage <- sc2bylineage %>% mutate_at(c("date","quarters","months","weeks","lineage","who"), as.character())
  
  varWeekly <- group_by_at(sc2bylineage,vars(weeks,lineage,who)) %>% summarise(.groups="keep",num = sum(num))
  names(varWeekly) <- c("date","lineage","who","num")
  varMonthly <- group_by_at(sc2bylineage,vars(months,lineage,who)) %>% summarise(.groups="keep",num = sum(num))
  names(varMonthly) <- c("date","lineage","who","num")
  varQuarterly <- group_by_at(sc2bylineage,vars(quarters,lineage,who)) %>% summarise(.groups="keep",num = sum(num))
  names(varQuarterly) <- c("date","lineage","who","num")
  
  weeklyVariantProp <- varWeekly[varWeekly$lineage %in% variantList,]
  sumNonVarData <- varWeekly[!varWeekly$lineage %in% variantList,c("date","num")] %>% summarise(num=sum(num))
  for(week in unique(sumNonVarData$date)){
    if(week %in% weeklyVariantProp$date){
      row <- data.frame(date = sumNonVarData[sumNonVarData$date == week,1],lineage="Other",who="Other",num = sumNonVarData[sumNonVarData$date == week,2])
      weeklyVariantProp <- bind_rows(weeklyVariantProp,row)
    }
  }
  
  monthlyVariantProp <- varMonthly[varMonthly$lineage %in% variantList,]
  sumNonVarData <- varMonthly[!varMonthly$lineage %in% variantList,c("date","num")] %>% summarise(num=sum(num))
  for(month in unique(sumNonVarData$date)){
    if(month %in% monthlyVariantProp$date){
      row <- data.frame(date = sumNonVarData[sumNonVarData$date == month,1],lineage="Other",who="Other",num = sumNonVarData[sumNonVarData$date == month,2])
      monthlyVariantProp <- bind_rows(monthlyVariantProp,row)
    }
  }
  
  quarterlyVariantProp <- varQuarterly[varQuarterly$lineage %in% variantList,]
  sumNonVarData <- varQuarterly[!varQuarterly$lineage %in% variantList,c("date","num")] %>% summarise(num=sum(num))
  for(quarter in unique(sumNonVarData$date)){
    if(quarter %in% quarterlyVariantProp$date){
      row <- data.frame(date = sumNonVarData[sumNonVarData$date == quarter,1],lineage="Other",who="Other",num = sumNonVarData[sumNonVarData$date == quarter,2])
      quarterlyVariantProp <- bind_rows(quarterlyVariantProp,row)
    }
  }
  
  return(list("weekly"=weeklyVariantProp,"monthly"=monthlyVariantProp,"quarterly"=quarterlyVariantProp))
}

#render a plot of the lineages sequenced by week/month/quarter
plotVariantTimeLineage <- function(data,label){
  ### use who labels
  if(label == 'Pangolin'){
    data_other <- data[data$lineage =="Other",]
    data <- data[data$lineage != "Other",]
    fig <- plot_ly()
    c = 1
    pallet = colorRampPalette(c("#320c55","#c18ff0"))(length(VBM_list))
    for(voi in VBM_list){
      data_holder <- data[data$lineage == voi,]
      fig <- fig %>% add_trace(
        type = "bar",
        x = data_holder$date,
        y = data_holder$num,
        name = voi,
        marker= list(color=pallet[c]),
        hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
        text = data_holder$num,
        textposition = "none"
      )
      c = c + 1
    }
    c = 1
    pallet = colorRampPalette(c("#880e0c","#f69593"))(length(VOC_list))
    for(voc in VOC_list){
      data_holder <- data[data$lineage == voc,]
      fig <- fig %>% add_trace(
        type = "bar",
        x = data_holder$date,
        y = data_holder$num,
        name = voc,
        marker= list(color=pallet[c]),
        hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
        text = data_holder$num,
        textposition = "none"
      )
      c = c + 1
    }
    fig <- fig %>% add_trace(
      type = "bar",
      x = data_other$date,
      y = data_other$num,
      name = data_other$lineage,
      marker= list(color="#CCCCCC"),
      hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
      text = data_other$num,
      textposition = "none"
    )
    
    fig <- fig %>% layout(
      barmode="stack",
      barnorm="percent",
      hoverlabel= list(
        font = list(
          size = 14
        )
      ),
      xaxis = list(
        categoryorder = "array",
        categoryarray = data$date
      ),
      yaxis = list(
        categoryorder = "category array",
        categoryarray = data$lineage
      )
    )
    return(fig)
  } else {
    #Collapse Data Num
    data <- data[,c('date','who','num')]
    data <- data %>% summarise(num=sum(num))
    data_other <- data[data$who == "Other",]
    data <- data[data$who != "Other",]
    fig <- plot_ly()
    #VOI
    pallet = colorRampPalette(c("#320c55","#c18ff0"))(length(WHO_VBM))
    c = 1
    for(voi in rev(WHO_VBM)){
      data_holder <- data[data$who == voi,]
      fig <- fig %>% add_trace(
        type = "bar",
        x = data_holder$date,
        y = data_holder$num,
        name = voi,
        marker= list(color=pallet[c]),
        hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
        text = data_holder$num,
        textposition = "none"
      )
      c = c + 1
    }
    #VOC
    c = 1
    pallet = colorRampPalette(c("#880e0c","#f69593"))(length(WHO_VOC))
    for(w in rev(WHO_VOC)){
      data_holder <- data[data$who == w,]
      fig <- fig %>% add_trace(
        type = "bar",
        x = data_holder$date,
        y = data_holder$num,
        name = w,
        marker= list(color=pallet[c]),
        hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
        text = data_holder$num,
        textposition = "none"
      )
      c = c + 1
    }
    fig <- fig %>% add_trace(
      type = "bar",
      x = data_other$date,
      y = data_other$num,
      name = data_other$who,
      marker= list(color="#CCCCCC"),
      hovertemplate = "%{x} \n %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
      text = data_other$num,
      textposition = "none"
    )
    
    fig <- fig %>% layout(
      barmode="stack",
      barnorm="percent",
      hoverlabel= list(
        font = list(
          size = 14
        )
      ),
      xaxis = list(
        categoryorder = "array",
        categoryarray = data$date
      ),
      yaxis = list(
        categoryorder = "category array",
        categoryarray = data$who
      )
    )
    return(fig)
  }
}

#selectable lineage by week/month/quarter
plotSelectedLineage <- function(data,lineages){
  fig <- plot_ly()
  
  #no lineages selected show all as other
  if(is.null(lineages)){
    data_other <- data
    data_other$lineage <- "Other"
    data_other <- group_by_at(data_other,vars(date,lineage)) %>% summarise(.groups="keep",num = sum(num))
  }
  #select lineages
  else{
    data_other <- data[!(data$lineage %in% lineages),]
    print(data_other[data_other$lineage == "B.1.1.7",])
    data_other$lineage <- "Other"
    data_other <- group_by_at(data_other,vars(date,lineage)) %>% summarise(.groups="keep",num = sum(num))
    c = 1
    pallet = colorRampPalette(c("#320c55","#c18ff0"))(length(lineages))
    for(lineage in lineages){
      data_holder <- data[data$lineage == lineage,]
      fig <- fig %>% add_trace(
        type = "bar",
        x = data_holder$date,
        y = data_holder$num,
        name = lineage,
        marker= list(color=pallet[c]),
        hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
        text = data_holder$num,
        textposition = "none"
      )
      c = c + 1
    }
  }
  fig <- fig %>% add_trace(
    type = "bar",
    x = data_other$date,
    y = data_other$num,
    name = "Other",
    marker= list(color="#CCCCCC"),
    hovertemplate = "%{x} \n Lineage: %{data.name} \n Number of Sequences: %{text} \n Percent of Sequences: %{y:.2f}<extra></extra>",
    text = data_other$num,
    textposition = "none"
  )
  
  fig <- fig %>% layout(
    barmode="stack",
    barnorm="percent",
    hoverlabel= list(
      font = list(
        size = 14
      )
    ),
    xaxis = list(
      categoryorder = "array",
      categoryarray = data$date
    ),
    yaxis = list(
      categoryorder = "category array",
      categoryarray = data$lineage
    )
  )
  return(fig)
}