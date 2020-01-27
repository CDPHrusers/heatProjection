#' get_rr
#'
#' This function allows you to filter a single year of OSHPD Emergency Department Visits for the Clinical Classification System Codes of interest.
#' @param BobbCodes A collection of text CCS codes to pull out.
#' @param file_path The file path to the ED data you wish to process.  Defaults to test data on the R server at CDPH
#' @param numDiagnosis If 1, only the primary diagnosis will be used. Values of 2 or 3 correspond to including secondary and tertiatry diagnoses. Defaults to 3.
#' @keywords EDvisits
#' @export
#' @examples
#' get_rr()

get_rr <- function(zipcode = "94706"){
  
  zipcode_index = which(names(meta.full$dlist_meta) == zipcode)
  
  varfun = "bs" 
  vardegree = 2 
  varper = c(10,75,90) 
  lag = 3
  lagnk = 2
  
  data <- meta.full$dlist[[zipcode_index]]
  blup <- meta.full$blup
  mintempzip <- meta.full$min_temp_zip
  
  argvar <- list(x=data$tmean_mean,fun=varfun,degree=vardegree,
                 knots=quantile(data$tmean_mean,varper/100,na.rm=T))
  bvar <- do.call(onebasis,argvar)
  pred <- crosspred(bvar,coef=blup[[zipcode_index]]$blup,vcov=blup[[zipcode_index]]$vcov,
                    model.link="log",by=0.1,cen=mintempzip[zipcode_index])
  
  return({
    data.table(
      zip = names(meta.full$dlist_meta[zipcode_index]),
      outcomes = stringr::str_sub(condition, 2),
      temp = names(pred$allRRfit),
      estimate_RR = pred$allRRfit,
      LL95_RR = pred$allRRlow,
      UL95_RR = pred$allRRhigh
    )
  })
  
}
