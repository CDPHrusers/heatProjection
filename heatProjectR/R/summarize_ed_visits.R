#' filter_ed
#'
#' This function allows you to filter a single year of OSHPD Emergency Department Visits for the Clinical Classification System Codes of interest.
#' @param BobbCodes A collection of text CCS codes to pull out.
#' @param file_path The file path to the ED data you wish to process.  Defaults to test data on the R server at CDPH
#' @param numDiagnosis If 1, only the primary diagnosis will be used. Values of 2 or 3 correspond to including secondary and tertiatry diagnoses. Defaults to 3.
#' @keywords EDvisits
#' @export
#' @examples
#' summarize_ed()


# 
# CCS codes 55  Fluid and electrolyte disorders
#           157 Renal failure (acute/unspecified)
#           159 Urinary tract infections
#           244 Heat stroke and other external causes
#           108 Congestive heart failure
#           2   Septicemia (except in labor)
# 


summarize_ed <-
  function(file_path = "//mnt/projects/ohe/heatProjections/data/ED/cdph_ed_rln2005.csv") {

    # read in Data table of ED for a single year
    foo <- fread(paste(file_path), select =  c("ccs_dx_prin", "patzip","serv_y", "patco"),  key = c("ccs_dx_prin"))[patco != 0]
    # setkey(foo, patzip, patco)
    
    # get rid of out of state and missing zip code records
    
    
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      foo <- foo[, .(n =.N) , by=.(ccs_dx_prin, patzip, serv_y)]
    return(foo)
  }

bobbCodes = c(55L, 157L, 159L, 244L, 108L, 2L)

ED_summaries <- rbindlist(lapply(list.files("//mnt/projects/ohe/heatProjections/data/ED/", full.names = T), summarize_ed))
fwrite(ED_summaries, "//mnt/projects/ohe/heatProjections/data/processed/ED_summaries_by_year_zip_css.csv")

ED_summaries[, .(EDvisits = sum(n)), by = .(ccs_dx_prin, serv_y)]
ED_summaries[ccs_dx_prin %in% bobbCodes, .(EDvisits = sum(n)), by = .(ccs_dx_prin, serv_y)]


ED_summaries[, .(EDvisits = sum(n)), by = .(patzip, serv_y)]
ED_summaries[ccs_dx_prin %in% bobbCodes, .(EDvisits = sum(n)), by = .(patzip, serv_y)]


merge({ED_summaries[, .(EDvisits = sum(n)), by = .(serv_y)]},
  {ED_summaries[ccs_dx_prin %in% bobbCodes, .(bobb_EDvisits = sum(n)), by = .(serv_y)]})

