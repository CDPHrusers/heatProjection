library(data.table)

filter_pdd <-
    function(bobbCodes = c(55L, 157L, 159L, 244L, 108L, 2L),
             file_path = "//mnt/projects/ohe/heatProjections/data/processed/ed_test_data.csv" ,
             numDiagnosis = 1) {
      
      
      
      
      columns_to_keep = c(
        # "dx_prin",
        "ccs_diagp",
        "ccs_odiag1",
        "ccs_odiag2",    # CCS of second other diagnosis
        # "fac_id",
        # "pat_type",
        # "lic_type",
        "agdyadm" ,
        "agyradm",   # time of encounter
        "sex",
        "patzip",
        "patcnty",
        # "serv_q",
        # "serv_d",
        # "serv_m",
        # "serv_y",
        # "dispn",
        # "payer",
        "proc_pdt",
        "pay_cat",
        "pay_type",
        "pay_plan",
        "charge", 
        # "opr1",
        "hplzip",
        "hplcnty",
        "rln",
        "race"
      )
      
      
      
    # read in Data table of ED for a single year
      foo <- fread(paste(file_path), select = columns_to_keep,  key = c("ccs_diagp",
                                                                        "ccs_odiag1",
                                                                        "ccs_odiag2"))[patcnty != 0]
      
      # setkey(foo, patzip, patco)
      
      # get rid of out of state and missing zip code records
      
      if (bobbCodes %in% c("all", "All", "ALL")) {
        ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
        ##bring in patient's zipcode, county code, and date of service
        foo <- foo[,  .(avg_charge = mean(charge, na.rm = T), 
                        n = .N, 
                        sd_charge = stats::sd(charge, na.rm = T)) , by =
                     .(patzip, proc_pdt)]
      }
      
      else if (numDiagnosis == 1) {
        ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
        ##bring in patient's zipcode, county code, and date of service
        foo <- foo[ccs_diagp %in% bobbCodes & charge >0 ,  .(avg_charge = mean(charge, na.rm = T), 
                                                n = .N, 
                                                sd_charge = stats::sd(charge, na.rm = T)) , by=.(ccs_diagp)]
      } 
      
      else
        echo("The number of diagnoses did not match the function criteria")
      return(foo)
    }
  

charges <- rbindlist(lapply(list.files("//mnt/projects/ohe/heatProjections/data/PDD/"), filter_pdd))

filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2005.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2006.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2007.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2008.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2009.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2010.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2011.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2012.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2013.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2014.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2015.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2016.csv", numDiagnosis = 1)
filter_pdd(file_path = "//mnt/projects/ohe/heatProjections/data/PDD/cdph_pdd_rln2017.csv", numDiagnosis = 1)


  