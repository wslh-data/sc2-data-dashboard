
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
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
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
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
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
  b16172_sub_lineages <- c("B.1.617.2","AY.1","AY.2","AY.3")
  # Sparkline 
  b117_spark_data <- as.data.frame(table(sc2Data[sc2Data$DOC > seq(as.Date(lastUpdate),length =2, by ="-2 months")[2] & sc2Data$Lineage == "B.1.1.7",c("DOC")]))
  b117hc <- hchart(b117_spark_data,"line",hcaes(Var1,Freq))%>% 
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  b1351_spark_data <- as.data.frame(table(sc2Data[sc2Data$DOC > seq(as.Date(lastUpdate),length =2, by ="-2 months")[2] & sc2Data$Lineage == "B.1.351",c("DOC")]))
  b1351hc <- hchart(b1351_spark_data,"line",hcaes(Var1,Freq))%>% 
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  p1_spark_data <- as.data.frame(table(sc2Data[sc2Data$DOC > seq(as.Date(lastUpdate),length =2, by ="-2 months")[2] & sc2Data$Lineage == "P.1",c("DOC")]))
  p1hc <- hchart(p1_spark_data,"line",hcaes(Var1,Freq))%>% 
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  b16172_spark_data <- as.data.frame(table(sc2Data[sc2Data$DOC > seq(as.Date(lastUpdate),length =2, by ="-2 months")[2] & sc2Data$Lineage %in% b16172_sub_lineages,c("DOC")]))
  b16172hc <- hchart(b16172_spark_data,"line",hcaes(Var1,Freq))%>% 
    hc_size(height = 50) %>% 
    hc_credits(enabled = FALSE) %>%
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
    value = nrow(sc2Data[sc2Data$Lineage %in% b16172_sub_lineages,]),
    subtitle = "B.1.617.2, AY.1, AY.2, AY.3",
    sparkobj = b16172hc,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports?pango=B.1.617.2"
  )
}

