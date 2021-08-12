
### Custom valueBox Function to show Sparklines
### Thanks to Joshua Kunst @jbkunst for figuring this out!
### https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
valueBoxSpark <- function(value, title=NULL, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, bgcolor = "#c5050c", textcolor="#FFFFFF", width = 4, href = NULL){
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = "small-box",
    style = paste0("background-color: ", bgcolor,";color: ",textcolor),
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0"),
    div(
      class = "inner",
      div(class="row",
        div(class="col-sm-3",style = "padding:0px",
          tags$small(title),
          h3(value)
        ),
        div(class="col-sm-9",style = "margin-top:25px",
          if (!is.null(sparkobj)) sparkobj
        )
      ),
      p(subtitle)
    )
    # bs3 icon-large
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}
### Custom Theme
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      enabled = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(color="#FFF",textOutline = "1px #c5050c")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        connectNulls=TRUE,
        color = "#FFFFFFBF",
        enableMouseTracking = FALSE,
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}




### Generate the Value Boxes
generateValueBoxPlots <- function() {
  ### Convert all B.1.617.2 sub-lineages to B.1.617.2
  vbdata <- sc2Data
  b16172_sub_lineages <- c("AY.1","AY.2","AY.3")
  vbdata$Lineage[vbdata$Lineage%in%b16172_sub_lineages] <- "B.1.617.2"
  
  ### Subset data to get last 2 months
  vbdata <- vbdata[vbdata$DOC > seq(as.Date(lastUpdate),length =2, by ="-2 months")[2],]
  
  ### Drop NAs
  vbdata <- na.omit(vbdata)
  
  ### Get summary data
  vbdata <- data.frame(table(vbdata$DOC,vbdata$Lineage))
  names(vbdata) <- c("date","lineage","num")
  vbdata <- group_by_at(vbdata,vars(date,lineage)) %>% summarise(.groups="keep",num = sum(num))
  
  ### Set missing variants to 0
  for( variant in VOC_list){
    if(! variant %in% vbdata$lineage){
      date_set <- unique(vbdata$date)
      for(d in date_set){
        df <- as.data.frame(t(c(
          date = d,
          lineage = variant,
          num = 0)))
        df$num <- as.numeric(df$num)
        vbdata <- rbind(vbdata,df)
      }
    }
  }
  
  ### Determine Proportions
  vbdata[,c("proportion")] <- 0
  for(date in vbdata$date){
    total <- sum(vbdata[vbdata$date == date,c("num")])
    vbdata[vbdata$date == date,c("proportion")] <- round((vbdata[vbdata$date == date,c("num")] / total) * 100,digits=0)
  }
  
  ### Create Line Plots 
  b117hc <- hchart(vbdata[vbdata$lineage == "B.1.1.7",],"spline",hcaes(date,proportion)) %>% 
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
    hc_yAxis(min=-15,max = 115) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 

  b1351hc <- hchart(vbdata[vbdata$lineage == "B.1.351",],"spline",hcaes(date,proportion)) %>% 
    hc_size(height = 50) %>%
    hc_credits(enabled = FALSE) %>%
    hc_yAxis(min=-15,max = 115) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 

  p1hc <- hchart(vbdata[vbdata$lineage == "P.1",],"spline",hcaes(date,proportion)) %>%  
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
    hc_yAxis(min=-15,max = 115) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  b16172hc <- hchart(vbdata[vbdata$lineage == "B.1.617.2",],"spline",hcaes(date,proportion)) %>%
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
    hc_yAxis(min=-15,max = 115) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  
  #### Variant Value Boxes
  b117 <<- valueBoxSpark(
    title = "Alpha",
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.1.7",]),
    subtitle = "B.1.1.7",
    icon = icon("virus"),
    sparkobj = b117hc,
    width = NULL,
    href = "https://outbreak.info/situation-reports?pango=B.1.1.7"
  )
  
  b1351 <<- valueBoxSpark(
    title = "Beta",
    value = nrow(sc2Data[sc2Data$Lineage == "B.1.351",]),
    subtitle = "B.1.351",
    sparkobj = b1351hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports?pango=B.1.351"
  )
  
  p1 <<- valueBoxSpark(
    title = "Gamma",
    value = nrow(sc2Data[sc2Data$Lineage == "P.1",]),
    subtitle = "P.1",
    sparkobj = p1hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports?pango=P.1"
  )
  
  b16172 <<- valueBoxSpark(
    title = "Delta",
    value = nrow(sc2Data[sc2Data$Lineage %in% c("B.1.617.2","AY.1","AY.2","AY.3"),]),
    subtitle = "B.1.617.2, AY.1, AY.2, AY.3",
    sparkobj = b16172hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports?pango=B.1.617.2"
  )
}

