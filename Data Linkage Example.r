# Data Linkage Example !!!!!!!!!!!!!!!!!!!!!!!!! 115 Rows, minimalst margins, probably just shy of 100 columns (99)
# About:
    # The following code is an example of the full data linkage process within a limited amount of
    # allowable space (two pages).

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
require(data.table) # For concise syntax and memory efficient code.
require(stringdist) # For phonetic algorithms (soundex, jaccard, jaro-winkler).

# # ---- Import datasets ----
# d1 <- fread("")
# d2 <- fread("")
# # Inspect the structures
# str(d1); str(d2)

d1 <- data.table(
    Given_Names = c("John Apple", "Jane Alex", "David", "Emily"),
    LAST_NAME = c("Smith", "Doe", "Johnson", "Brown"),
    Address = c("123 Main St, 4000", "456 Elm St 4059", "789 Oak St 4001", "321 Pine St 4002"),
    DoB = c("1991-11-21", "1995-02-15", "1985-06-30", "1998-11-20"),
    Gender = c("Male", "F", "MALE", "f"),
    Alias_given_names = c("Johnny", "Janie", "Dave", "Em"),
    Alias_last_NameS = c("Smi", "D", "John", "B")
)

d2 <- data.table(
    given_names = c("Johnny Apple", "Jane A", "David", "Emily"),
    last_name = c("Smyth", "Alex-Smith", "Johnson", "Brown"),
    address = c("123 Mains St, 4000", "456 Elm St 4059", "789 Oak St 4001", "321 Pine St 4002"),
    DoB = c("1990-11-12", "1995-02-15", "1985-06-30", "1998-11-20"),
    Gender = c("Male", "F", "MALE", "f"),
    Alias_given_names = c("John", "Janie", "Dave", "Em"),
    Alias_last_NameS = c("Smi", "D", "John", "B")
)


# ---- Clean, Standardise and Wrangle datasets ----
# Create an unique ID on each dataset.
d1[, id := .I]
d2[, id := .I]

# Variables names.
setnames(d1, tolower)
setnames(d2, tolower)
# Standardise `gender`.
d1[, gender := toupper(gsub("^([A-z]).*", "\\1", gender))]
d2[, gender := toupper(gsub("^([A-z]).*", "\\1", gender))]
# Loop over each variable to normalise
for (i in names(d1)) {
    # Convert all character variables to a consistent case and remove redundant white space.
    d1[, (i) := trimws(toupper(get(..i)))] 
    d2[, (i) := trimws(toupper(get(..i)))]
}
# Derive a postcode from the `address` variable.
d1[, postcode := gsub(".*(\\d{4}).*", "\\1", address)]
d2[, postcode := gsub(".*(\\d{4}).*", "\\1", address)]

# ---- Create blocks and Link datasets ----
t <- rbind(d1, d2, idcol = "dataset")

t <- melt(t,
    id.vars = c("id", "dataset", "dob", "gender", "address", "postcode"),
    measure.vars = patterns("given_names" = "given_name", "last_name" = "last_name"),
    variable.factor = F
)
t[, id := paste0(id, "_", variable)]

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







