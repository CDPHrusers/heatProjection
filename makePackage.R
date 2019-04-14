install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)



setwd("R:/heatProjections/code/heatProjection/")

create("heatProjectR")

setwd("./heatProjectR/")
document()


setwd("..")
install("heatProjectR")


?getPRISM
