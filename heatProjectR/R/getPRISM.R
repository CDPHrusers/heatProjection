#' Get PRISM Function
#'
#' This function allows you to ingest a single year of PRISM daily data processed at the zip code scale in Google EARTH.
#' @param year Four digit year as appears in the PRISM filename 'prism_YYYY.csv'. 
#' @keywords PRISM
#' @export
#' @examples
#' getPRISM()


getPRISM <- function(year){
  #year<-2008
  file<- paste0("PRISM/prism_",year,".csv")
  foo <- fread(paste(file))
  #colnames(foo)[1]<-"index"
  
  foo$Date <-as.Date(substr(foo$`system:index`, start = 1,stop = 8), format = "%Y%m%d")
  
  return(foo)
  
}