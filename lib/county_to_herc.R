
hercRegions = list(
  "Northwest"=c("douglas","bayfield","ashland","burnett","washburn","sawyer","polk","barron","rusk",
                "saint croix","dunn","chippewa","pierce","pepin","eau claire"),
  "North Central"=c("iron","vilas","price","oneida","forest","taylor","lincoln","langlade","clark","marathon","wood","portage"),
  "Northeast"=c("florence","marinette","oconto","door","brown","kewaunee","manitowoc"),
  "Western"=c("buffalo","trempealeau","jackson","la crosse","monroe","vernon","crawford"),
  "South Central"=c("juneau","adams","marquette","richland","sauk","columbia","dodge","grant","iowa","dane","jefferson","lafayette","green","rock"),
  "Fox Valley Area"=c("menominee","shawano","waupaca","outagamie","waushara","winnebago","calumet","green lake"),
  "Southeast"=c("fond du lac","sheboygan","washington","ozaukee","waukesha","milwaukee","walworth","racine","kenosha")
)
CountyToHERC <- function(counties){
  counties <- as.list(counties)
  for(item in names(hercRegions)){
    counties <- rapply(counties, function(c) ifelse(c%in%hercRegions[[item]],item,c), how = "replace")
  }
  return(unlist(counties))
}
