library(paws)

get_DHS_county_data <- function(){
  svc <- dynamodb()
  
  wi_counties <- c(
    "Adams","Ashland","Barron","Bayfield","Brown","Buffalo","Burnett","Calumet","Chippewa","Clark","Columbia",
    "Crawford","Dane","Dodge","Door","Douglas","Dunn","Eau Claire","Florence","Fond du Lac","Forest","Grant",
    "Green","Green Lake","Iowa","Iron","Jackson","Jefferson","Juneau","Kenosha","Kewaunee","La Crosse","Lafayette",
    "Langlade","Lincoln","Manitowoc","Marathon","Marinette","Marquette","Menominee","Milwaukee","Monroe","Oconto",
    "Oneida","Outagamie","Ozaukee","Pepin","Pierce","Polk","Portage","Price","Racine","Richland","Rock","Rusk",
    "St. Croix","Sauk","Sawyer","Shawano","Sheboygan","Taylor","Trempealeau","Vernon","Vilas","Walworth","Washburn",
    "Washington","Waukesha","Waupaca","Waushara","Winnebago","Wood")
  
  data = list()
  
  # get last date of data
  last_date = NA
  c = 0
  while(is.na(last_date)){
    key = paste(Sys.Date()-c,"Dane",sep="_")
    hit <- svc$query("DHS-SC2-County-Data",
                     KeyConditionExpression = "date_county = :value",
                     ExpressionAttributeValues = list(':value' = list('S' = key)))
    if(hit$Count > 0){
      last_date = Sys.Date()-c
    }
    c = c + 1
  }
  
  for(item in wi_counties){
    key = paste(last_date,item,sep="_")
    hit <- svc$query("DHS-SC2-County-Data",
                     KeyConditionExpression = "date_county = :value",
                     ExpressionAttributeValues = list(':value' = list('S' = key))
    )
    data <- c(data,hit$Items)
  }
  
  df <- data.frame(matrix(ncol=2,nrow=0))
  colnames(df) <- c('County','Positives')
  for(i in data){
    county <- i$NAME$S
    cases <- i$POSITIVE$N
    df[nrow(df)+1,] <- c(county,cases)
    df$Positives <- as.numeric(df$Positives)
  }
  
  return(df)
}

get_GISAID_Metadata_data <- function(){
  svc <- dynamodb()
  projexp = "covv_accession_id,covv_collection_date,covv_lineage,covv_location,covv_subm_date,covv_subm_lab,pangolin_lineages_version"
  data <- svc$scan("SC2-Dashboard-GISAID",ProjectionExpression=projexp)
  gisaid_data <- list()
  while(TRUE){
    gisaid_data <- c(gisaid_data,data$Items)
    if(is.null(data$LastEvaluatedKey$`GISAID-ID`$S)){
      break
    }
    data <- svc$scan("SC2-Dashboard-GISAID",ExclusiveStartKey = data$LastEvaluatedKey,ProjectionExpression = projexp)
  }
  
  df <- data.frame(matrix(ncol=7))
  colnames(df) <- c('GISAID_ID','DOC','Location','Pangolin','PangoVersion','DOS','SubLAB')
  for(i in gisaid_data){
    lineage <- i$covv_lineage$S
    location <- i$covv_location$S
    doc <- i$covv_collection_date$S
    dos <- i$covv_subm_date$S
    id <- i$covv_accession_id$S
    sublab <- i$covv_subm_lab$S
    pangover <- i$pangolin_lineages_version$S
    df[nrow(df)+1,] <- c(id,doc,location,lineage,pangover,dos,sublab)
  }
  df <- na.omit(df)
  return(df)
}