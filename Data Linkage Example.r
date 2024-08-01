# Data Linkage Example !!!!!!!!!!!!!!!!!!!!!!!!! 115 Rows, minimalst margins, probably just shy of 100 columns (99)
# Code & data available: https://github.com/KyleHaynes/r_linkage_example
# About:
    # The following code is an example of the full data linkage process within a limited amount of
    # allowable space (two pages).

    # Like all linkages context and scope are required
    # Context is: a small number of records from another area are unable to be linked across two
    # internal datasets, tollerence is high

    # Need to talk about the purpose of the linkages --- what am i linking?

# Steps:
    # Load required packages
    # Import datasets
    # Clean, Standardise and Wrangle datasets
    # Create blocks and Link datasets
    # Perform comparisons
    # Apply thresholds
    # Export linkage keys

# ---- Load required packages ----
# install.packages(c("data.table", "stringdist"))
require(data.table) # For concise syntax & memory efficient code.
require(stringdist) # For phonetic algorithms.

# # ---- Import datasets ----
d1 <- fread("https://raw.githubusercontent.com/KyleHaynes/r_linkage_example/main/data_1.tsv")
d2 <- fread("https://raw.githubusercontent.com/KyleHaynes/r_linkage_example/main/data_2.tsv")
# Inspect the structures
str(d1); str(d2)

# ---- Clean, Standardise and Wrangle datasets ----
# IDs should always be unique.
anyDuplicated(c(d1$id, d2$id)) # All unique.

# Standardise variables names.
setnames(d1, tolower)
setnames(d2, tolower)
# Check they're identical.
all(names(d1) == names(d2)) # They are.

# We can bind the datasets and clean, standardise as one dataset.
d <- rbind(d1, d2, idcol = "data_source")

# Standardise `gender`.
d[, gender := toupper(gsub("^([A-z]).*", "\\1", gender))]

# Loop over each variable to normalise (consistent case, remove redundant white space).
for (i in names(d)) {
    d[, (i) := trimws(toupper(get(..i)))]
}

# Derive a postcode from the `address` variable.
d[, postcode := gsub(".*(\\d{4}).*", "\\1", address)]

# We have alias names for some. We can melt and link alias names with names (and vice versa). 
d <- melt(d,
    id.vars = c("id", "data_source", "dob", "gender", "address", "postcode"),
    measure.vars = patterns("given_names" = "given_name", "last_name" = "last_name"),
    variable.factor = F
)
t[, id := paste0(id, "_", variable)]


# ---- Create Blocks and Link datasets ----

t[, block_1 := paste(year(dob), phonetic(given_names, method = "soundex"), sep = "~")]
t[, block_2 := paste(year(dob), phonetic(last_name, method = "soundex"), sep = "~")]
t[, block_3 := paste(postcode, phonetic(given_names, method = "soundex"), sep = "~")]

# We can now split the datasets back out to apply the blocking rules
d1 <- split(t[, -c("dataset", "variable")], t$dataset)[[1]]
d2 <- split(t[, -c("dataset", "variable")], t$dataset)[[2]]

bp <- rbind(
    merge(d1[, -c("block_2", "block_3")], d2[, -c("block_2", "block_3")], 
          by.x = "block_1", by.y = "block_1", all = T, suffixes = c("_d1", "_d2")),
    merge(d1[, -c("block_1", "block_3")], d2[, -c("block_1", "block_3")], 
          by.x = "block_2", by.y = "block_2", all = T, suffixes = c("_d1", "_d2")),
    merge(d1[, -c("block_1", "block_2")], d2[, -c("block_1", "block_2")], 
          by.x = "block_3", by.y = "block_3", all = T, suffixes = c("_d1", "_d2")),
    fill = T
)[, -c("block_1", "block_2", "block_3")][!is.na(id_d1) & !is.na(id_d2)]
setcolorder(bp, c(rbind(grep("_d1$", names(bp), value = T), grep("_d2$", names(bp), value = T))))

bp <- unique(bp)

# Derive string comparisons
bp[, `:=`(
    jaro_given_name = 1 - stringdist(given_names_d1, given_names_d2, method = "jw"),
    jaro_last_name = 1 - stringdist(last_name_d1, last_name_d2, method = "jw"),
    postcode_match = postcode_d1 == postcode_d2,
    gender_match = gender_d1 == gender_d2,
    jac_address = 1 - stringdist(address_d1, address_d2, method = "jaccard", q = 2)
)]



# ---- Perform comparisons ----


# ---- Apply thresholds ----


# ---- Export linkage keys ----
#







