#' Combine Ed and Temp Function
#'
#' This function allows you to ingest a single year of PRISM daily data processed at the zip code scale in Google EARTH.
#' @param file_path_ed  The file path to the PRISM data you wish to process.  Defaults to test data on the R server at CDPH. 
#' @keywords PRISM
#' @export
#' @examples
#' get_prism()

combine_ed_temp <- function(file_path_ed = "R:/heatProjections/data/processed/ed_test_data.csv", file_path_prism = "R:/heatProjections/data/processed/prism_test_data.csv", output_path = "data/processed/combined_test_data.csv") {
  ED <- filter_ed(file_path = file_path_ed)
  temp <- get_prism(file_path = file_path_prism)
  
  #merge the ED data with the temp data by zipcode and date
  join<-merge(temp, ED, by.x = c("Date", "ZCTA"), by.y = c("Date", "patzip"), all.x = T) %>% replace_na(list(n = 0))
  
  fwrite(join, output_path)
  
  return(join)
  
}
