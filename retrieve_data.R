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
  for(item in wi_counties){
    key = paste(Sys.Date()-1,item,sep="_")
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