#' Get PRISM Function
#'
#' This function allows you to ingest a single year of PRISM daily data processed at the zip code scale in Google EARTH.
#' @param file_path  The file path to the PRISM data you wish to process.  Defaults to test data on the R server at CDPH. 
#' @keywords PRISM
#' @export
#' @examples
#' get_prism()


get_prism <- function(file_path = "R:/heatProjections/data/processed/prism_test_data.csv"){
   foo <- fread(paste(file_path))
  #colnames(foo)[1]<-"index"
  
  foo$Date <-as.Date(substr(foo$`system:index`, start = 1,stop = 8), format = "%Y%m%d")
  
  return(foo)
  
}