##code to create a subset of data for testing functions

data<- fread("/data/tempAndED_allYears.csv")

# sample 5% of data
test_index <- sample(1:nrow(data), 0.05 * nrow(data), replace=FALSE)

# subset data
sub_data <- data[test_index,]

fwrite(sub_data, "/processed/sub_tempAndED_allYears.csv")
