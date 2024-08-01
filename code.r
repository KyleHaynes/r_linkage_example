install.packages("data.table")

require(data.table)

# Use data.table's `fread` function to read in the two datasets.
d1 <- fread("https://raw.githubusercontent.com/KyleHaynes/r_linkage_example/main/data_1.csv")
