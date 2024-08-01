# Data Linkage Example
# About: The following code is an example of the full data linkage process within a limited amount of
#        allowable space (two pages). Code available: https://github.com/KyleHaynes/r_linkage_example
# Steps:
# 1. Load required packages | 2. Import & inspect data | 3. Clean, Standardise & Wrangle datasets
# 4. Create blocks & Link | 5. Perform comparisons | 6. Apply thresholds | 7. Export linkage keys

# ---- 1. Load required packages ----
require(data.table) # For concise syntax & memory efficient code.
require(stringdist) # For phonetic algorithms.

# ---- 2. Import & inspect data ----
d1 <- fread("https://raw.githubusercontent.com/KyleHaynes/r_linkage_example/main/data_1.tsv", na = "")
d2 <- fread("https://raw.githubusercontent.com/KyleHaynes/r_linkage_example/main/data_2.tsv", na = "")
# Inspect the structures.
str(d1); str(d2)
# Cleaning is required. Variable names are inconsistent, data is not standardised.
# IDs should always be unique.
anyDuplicated(c(d1$id, d2$id)) # All unique.

# ---- 3. Clean, Standardise and Wrangle data ----
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
for (i in names(d)) { d[, (i) := trimws(toupper(get(..i)))] }
# Derive a postcode from the `address` variable.
d[, postcode := gsub(".*(\\d{4})$", "\\1", address)]
# We have alias names for some records. We can melt & link alias names with names (and vice versa). 
d <- melt(d,
    id.vars = c("id", "data_source", "dob", "gender", "address", "postcode"),
    measure.vars = patterns("given_names" = "given_name", "last_name" = "last_name"),
    variable.factor = F
    )[!is.na(given_names) & !is.na(last_name)]

# ---- 4. Create Blocks & Link ----
d[, block_1 := paste(year(dob), phonetic(given_names, method = "soundex"), sep = "~")]
d[, block_2 := paste(year(dob), phonetic(last_name, method = "soundex"), sep = "~")]
d[, block_3 := paste(postcode, dob, sep = "~")]
# Melt again by the blocking variables. This ensures we only need to merge once rather than 3 times.
d <- melt(d,
    id.vars = c("id", "data_source", "given_names", "last_name", "dob", "gender", "address", "postcode"),
    measure.vars = patterns("blocks" = "block_"),
    variable.factor = F
)[, -c("variable")]

# We can now split the datasets back into individual datasets and link/merge on the blocking vars.
d1 <- split(d[, -c("data_source")], d$data_source)[[1]]
d2 <- split(d[, -c("data_source")], d$data_source)[[2]]
# Merge/Link the datasets by the various blocks created.
bp <- merge(d1, d2, by = "blocks", 
    all = T, suffixes = c("_d1", "_d2"))[, -c("blocks")][!is.na(id_d1) & !is.na(id_d2)]
setcolorder(bp, c(rbind(grep("_d1$", names(bp), value = T), grep("_d2$", names(bp), value = T))))
bp <- unique(bp)

# ---- Perform comparisons ----
# Derive string comparisons.
bp[, `:=`(
    jaro_given_name = 1 - stringdist(given_names_d1, given_names_d2, method = "jw"),
    jaro_last_name = 1 - stringdist(last_name_d1, last_name_d2, method = "jw"),
    jaro_dob = 1 - stringdist(dob_d1, dob_d2, method = "jw"),
    gender_match = gender_d1 == gender_d2,
    postcode_match = postcode_d1 == postcode_d2,
    jac_address = 1 - stringdist(address_d1, address_d2, method = "jaccard", q = 2)
)]

# ---- Apply thresholds ----
# Inspect the data to determine thresholds.
View(bp)
# Apply thresholds to attempt to identify True Positives.
View(bp[l <<- jaro_given_name >= .82 & jaro_last_name >= .8 & jaro_dob == 1])
# All those matches look good. Flag as matched.
bp[, matched_pairs := fifelse(l, T, F)]
# Have we missed anything?
View(bp[(!matched_pairs)])

# ---- Export linkage keys ----
bp[(matched_pairs), .(id_d1, id_d2)]

    # Like all linkages context and scope are required
    # Context is: a small number of records from another area are unable to be linked across two
    # internal datasets, tollerence is high
    # Need to talk about the purpose of the linkages --- what am i linking?

# How the code could be improved
- Version control. This is something that I do in all projects, however, not feasible 
- Checks / cleaning of the data would be more involved
- Formatting of the code, more space to allow QAers to easily digest the code.
- Datasets would not generally be combined and cleaned
- With regards to missed links. A second round of linkage on the residual with extremely loose blocks
- A technical report (if required). Showing explantory notes on how the data was linkaged
- QA, I pride myself on code, however, am always keen to collaborate and receive constructive feedback.
    - This would include QA through Gitlab




