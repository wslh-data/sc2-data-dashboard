
### Custom valueBox Function
### Thanks to Joshua Kunst @jbkunst for figuring this out!
### https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
valueBoxRecent <- function(value, title=NULL, recent_value = NULL, subtitle, info = NULL, trend = NULL,
                          icon = NULL, bgcolor = "#c5050c", textcolor="#FFFFFF", width = 4, href = NULL){
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = "small-box",
    style = paste0("background-color: ", bgcolor,";color: ",textcolor),
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0"),
    div(class = "inner",
      div(class="row",style="display:flex;align-items:center;",
          div(class="col-sm-8",style = "padding:0px",
              tags$b(tags$small(title)),tags$br(),
              tags$small("Last 30 Days: "),tags$b(recent_value),tags$br(),
              tags$small("Total: "),tags$b(value),tags$br(),
              tags$small(style="font-size:65%",subtitle)
              ),
          div(class="col-sm-4", style = "padding:0px",
              if (!is.null(trend)) div(class="text-center",style="font-size:52px;",icon(trend))
              )
      )
    )
  )
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

### Trend Arrow
determineTrend <- function(start,end){
  if(start < 1 & end < 1){
    return("minus")
  }
  if(start < 1 & end < 10){
    return("minus")
  }
  if(start < 1 & end >= 10){
    return("arrow-up")
  }
  percent_change <- ((end-start)/start)*100
  
  if(percent_change > 25) {
    return("arrow-up")
  }
  if(percent_change < -25) {
    return("arrow-down")
  }
  return("minus")
}

### Generate the Value Boxes
generateValueBoxPlots <- function() {
  vbdata <- na.omit(sc2Data)
  
  vbdata$Lineage[which(vbdata$Lineage%in%WHO_list$Alpha)] <- "Alpha"
  vbdata$Lineage[which(vbdata$Lineage%in%WHO_list$Beta)] <- "Beta"
  vbdata$Lineage[which(vbdata$Lineage%in%WHO_list$Gamma)] <- "Gamma"
  vbdata$Lineage[which(vbdata$Lineage%in%WHO_list$Delta)] <- "Delta"
  vbdata$Lineage[which(vbdata$Lineage%in%WHO_list$Omicron)] <- "Omicron"
  
  ### Get counts
  alpha_counts <- nrow(vbdata[which(vbdata$Lineage == "Alpha"),])
  beta_counts <- nrow(vbdata[which(vbdata$Lineage == "Beta"),])
  gamma_counts <- nrow(vbdata[which(vbdata$Lineage == "Gamma"),])
  delta_counts <- nrow(vbdata[which(vbdata$Lineage == "Delta"),])
  omicron_counts <- nrow(vbdata[which(vbdata$Lineage == "Omicron"),])
  
  alpha_recent_counts <- nrow(vbdata[which(vbdata$Lineage == "Alpha" & as.Date(vbdata$DOC) >= as.Date(lastUpdate)-30),])
  beta_recent_counts <- nrow(vbdata[which(vbdata$Lineage == "Beta" & as.Date(vbdata$DOC) >= as.Date(lastUpdate)-30),])
  gamma_recent_counts <- nrow(vbdata[which(vbdata$Lineage == "Gamma" & as.Date(vbdata$DOC) >= as.Date(lastUpdate)-30),])
  delta_recent_counts <- nrow(vbdata[which(vbdata$Lineage == "Delta" & as.Date(vbdata$DOC) >= as.Date(lastUpdate)-30),])
  omicron_recent_counts <- nrow(vbdata[which(vbdata$Lineage == "Omicron" & as.Date(vbdata$DOC) >= as.Date(lastUpdate)-30),])
  
  #### Variant Value Boxes
  b117 <<- valueBoxRecent(
    title = "Alpha",
    value = alpha_counts,
    subtitle = "B.1.1.7, Q.1 - Q.8",
    icon = icon("virus"),
    recent_value = alpha_recent_counts,
    width = NULL,
    href = "https://outbreak.info/situation-reports/alpha"
  )
  
  b1351 <<- valueBoxRecent(
    title = "Beta",
    value = beta_counts,
    subtitle = "B.1.351, B.1.351.2, B.1.351.3",
    recent_value = beta_recent_counts,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/beta"
  )
  
  p1 <<- valueBoxRecent(
    title = "Gamma",
    value = gamma_counts,
    subtitle = "P.1, P.1.1, P.1.2",
    recent_value = gamma_recent_counts,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/gamma"
  )
  
  b16172 <<- valueBoxRecent(
    title = "Delta",
    value = delta_counts,
    subtitle = "B.1.617.2, All AY",
    recent_value = delta_recent_counts,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/delta"
  )
  
  b11529 <<- valueBoxRecent(
    title = "Omicron",
    value = omicron_counts,
    subtitle = "B.1.1.529,BA.1,BA.2,BA.3",
    recent_value = omicron_recent_counts,
    icon = icon("virus"),
    width = NULL,
    href = "https://outbreak.info/situation-reports/omicron"
  )
}

