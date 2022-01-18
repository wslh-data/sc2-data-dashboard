
get_lineage_data <- function(){
  svc <- dynamodb()
  projexp = "lineage"
  data <- svc$scan("pango_designations",ProjectionExpression=projexp)
  lineage_data <- list()
  while(TRUE){
    lineage_data <- c(lineage_data,data$Items)
    if(is.null(data$LastEvaluatedKey$`lineage`$S)){
      break
    }
    data <- svc$scan("pango_designations",ExclusiveStartKey = data$LastEvaluatedKey,ProjectionExpression = projexp)
  }
  
  df <- data.frame(matrix(ncol=1))
  colnames(df) <- c('lineages')
  for(i in lineage_data){
    lineages <- i$lineage$S
    df[nrow(df)+1,] <- c(lineages)
  }
  df <- na.omit(df)
  return(df)
}

get_DHS_county_data <- function(dynamodb_table){
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
    hit <- svc$query(dynamodb_table,
                     KeyConditionExpression = "date_county = :value",
                     ExpressionAttributeValues = list(':value' = list('S' = key)))
    if(hit$Count > 0){
      last_date = Sys.Date()-c
    }
    c = c + 1
  }
  
  for(item in wi_counties){
    key = paste(last_date,item,sep="_")
    hit <- svc$query(dynamodb_table,
                     KeyConditionExpression = "date_county = :value",
                     ExpressionAttributeValues = list(':value' = list('S' = key))
    )
    data <- c(data,hit$Items)
  }
  
  df <- data.frame(matrix(ncol=2,nrow=0))
  colnames(df) <- c('County','Positives')
  for(i in data){
    county <- i$GEOName$S
    cases <- i$POS_CUM_CONF$N
    df[nrow(df)+1,] <- c(county,cases)
    df$Positives <- as.numeric(df$Positives)
  }
  
  return(df)
}

get_GISAID_Metadata_data <- function(){
  svc <- athena()
  query <- "SELECT covv_accession_id,
    covv_collection_date,
    covv_lineage,
    covv_location,
    covv_subm_date,
    covv_subm_lab,
    pangolin_lineages_version 
FROM \"covid-dashboard\".\"gisaid\" 
WHERE covv_location LIKE 'North America / USA / Wisconsin%';"
  query_exe <- svc$start_query_execution(QueryString = query)
  Sys.sleep(5)
  status <- svc$get_query_execution(query_exe)
  state =  status$QueryExecution$Status$State
  while(state == 'RUNNING' | state == 'QUEUED'){
    Sys.sleep(5)
    status <- svc$get_query_execution(query_exe)
    state =  status$QueryExecution$Status$State
  }
  if(state == 'SUCCEEDED'){
    svc <- s3()
    key = paste('query_results/',query_exe$QueryExecutionId,'.csv',sep='')
    data <- svc$get_object(Bucket='sars-cov-2-dashboard',Key=key)
    df <- read.csv(text=readBin(data$Body,character()))
    colnames(df) <- c('GISAID_ID','DOC','Lineage','Location','DOS','SubLAB','PangoVersion')
    df <- na.omit(df)
    return(df)
  }
  else{
    return()
  }
}