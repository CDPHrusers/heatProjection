library(data.table)
library(tidyr)
library(lubridate)
gc()
# setwd("R:/heatProjections/")
setwd("//mnt/projects/ohe/heatProjections/")

# or through Rstudio in your browser 
# setwd("//mnt/projects/ohe/heatProjections/")
source("code/heatProjection/heatProjectR/R/filter_ed.R")
source("code/heatProjection/heatProjectR/R/get_prism.R")
source("code/heatProjection/heatProjectR/R/combine_ed_temp.R")
source("code/heatProjection/heatProjectR/R/first_stage_DLNM.R")
source("code/heatProjection/heatProjectR/R/meta_stage_DLNM.R")


foo <- fread("./data/processed/temp_and_ed_05-17.csv")

goo <- foo[ZCTA %in% c(90017, 92102, 92113, 93301, 90073, 90263, 91210)]


plotData <- foo[,temp := round((tmean_mean*9/5)+32, 0)] %>%
  .[,year := year(Date)] %>% 
  .[, .(count = sum(n, na.rm=T)), by = .(ZCTA, temp, year)] 

fwrite(plotData, "./HeatProjectionVewer/zipcodes_edvisits_by_year_temp.csv")


plotData[ZCTA == 90201]%>% 
    ggplot() + geom_bar(aes(x=temp, y = count, fill = temp), stat="identity") + facet_wrap(~ year) + scale_fill_gradient2(low = "blue", high = "red", mid = "gray90", midpoint = 65)

foo[,YR := year(Date)] %>%
  .[, .(count = sum(n, na.rm=T)), by = .(ZCTA, YR)] %>% 
  dcast.data.table(formula = ZCTA ~ YR)


library(sf)


install.packages("rmapshaper")


# zips <-  st_read("./HeatProjectionVewer/zipcodes.geojson") %>% rename(ZCTA = ZCTA5CE10) %>% select(ZCTA)
zips <-  st_read("./HeatProjectionVewer/CAzips.shp") %>% rename(ZCTA = ZCTA5CE10) %>% select(ZCTA) %>% lwgeom::st_transform_proj(crs =" +proj=longlat +datum=WGS84 +no_defs")

boo <- foo[,YR := year(Date)] %>%
    .[, .(count = sum(n, na.rm=T)), by = .(ZCTA, YR)] %>% 
    dcast.data.table(formula = ZCTA ~ YR) %>%
  .[,allYears := `2005` + `2006` + `2007` + `2008` + `2009` + `2010` + `2011` + `2012` + `2013` + `2014` + `2015` + `2016` + `2017`]
    
merge(zips, boo) %>% saveRDS("./HeatProjectionVewer/zipcodes_edvisits_spatial.RDS")
  
  
  st_write("./HeatProjectionVewer/zipcodes_edvisits.geojson",delete_dsn=TRUE)

  

  
  
  foo