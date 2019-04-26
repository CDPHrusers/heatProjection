setwd("R:/heatProjections/code/heatProjection/")
library(devtools)
library(heatProjectR)
library(data.table)

setwd("R:/heatProjections/data/")

# read in real ED data
actual_ed <- fread("ED/cdph_ed_rln2005.csv")
# read in Prism Data
actual_prism <- fread("PRISM/prism_2005.csv")

random_zips <- sample(unique(actual_prism$ZCTA), 25)


foo <- actual_ed[sample(nrow(actual),2000),]
# order subset by Date
foo2<-foo[order(serv_dt),]
# pull out the age at service and sort it
ages<-sort(foo2$agyrserv)
foo2$agyrserv <- ages
# pull out the age at service and sort it
dobs<-sort(foo2$brthdate)
foo2$brthdate <- dobs
# pull out the age at service and sort it
dobs_raw<-sort(foo2$dob_raw)
foo2$dob_raw <- dobs_raw

# pull out the age at service and sort it
dobs_raw<-sort(foo2$dob_raw)
foo2$dob_raw <- dobs_raw

# remove zipcode and age at service from subsetted data
foo2$patzip <- random_zips
foo2$faczip <- random_zips

# save as sample data to test functions on
fwrite(foo2, "processed/ed_test_data.csv")

pris <- actual_prism[ZCTA %in% random_zips]

fwrite(pris, "processed/prism_test_data.csv")




