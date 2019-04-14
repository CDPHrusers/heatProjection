getPRISM <- function(year){
  #year<-2008
  file<- paste0("PRISM/prism_",year,".csv")
  foo <- fread(paste(file))
  #colnames(foo)[1]<-"index"
  
  foo$Date <-as.Date(substr(foo$`system:index`, start = 1,stop = 8), format = "%Y%m%d")
  
  return(foo)
  
}