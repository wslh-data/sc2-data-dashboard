
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
  vbdata <- sc2Data
  
  ### Group all sub-lineages into parents
  b16172_sl <- c("B.1.617.2","AY.1","AY.2","AY.3","AY.3.1","AY.4",
                 "AY.5","AY.5.1","AY.5.2","AY.6","AY.7","AY.7.1",
                 "AY.7.2","AY.8","AY.9","AY.10","AY.11","AY.12",
                 "AY.13","AY.14","AY.15","AY.16","AY.17","AY.18",
                 "AY.19","AY.20","AY.21","AY.22","AY.23","AY.24",
                 "AY.25")
  b1351_sl <- c("B.1.351.2","B.1.351.3")
  p1_sl <- c("P.1.1","P.1.2")
  
  vbdata$Lineage[vbdata$Lineage%in%b16172_sl] <- "B.1.617.2"
  vbdata$Lineage[vbdata$Lineage%in%b1351_sl] <- "B.1.351"
  vbdata$Lineage[vbdata$Lineage%in%p1_sl] <- "P.1"
  
  ### Get counts
  alpha_counts <- nrow(vbdata[vbdata$Lineage == "B.1.1.7",])
  beta_counts <- nrow(vbdata[vbdata$Lineage == "B.1.351",])
  gamma_counts <- nrow(vbdata[vbdata$Lineage == "P.1",])
  delta_counts <- nrow(vbdata[vbdata$Lineage == "B.1.617.2",])
  
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
    value = alpha_counts,
    subtitle = "B.1.1.7",
    icon = icon("virus"),
    sparkobj = b117hc,
    width = NULL,
    href = "https://outbreak.info/situation-reports/alpha"
  )
  
  b1351 <<- valueBoxSpark(
    title = "Beta",
    value = beta_counts,
    subtitle = "B.1.351, B.1.351.2, B.1.351.3",
    sparkobj = b1351hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/beta"
  )
  
  p1 <<- valueBoxSpark(
    title = "Gamma",
    value = gamma_counts,
    subtitle = "P.1, P.1.1, P.1.2",
    sparkobj = p1hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/gamma"
  )
  
  b16172 <<- valueBoxSpark(
    title = "Delta",
    value = delta_counts,
    subtitle = "B.1.617.2, AY.1 - AY.25",
    sparkobj = b16172hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/delta"
  )
}

