# ---- 1. Load required packages ----
# - R is good, however, it requires packages to be really good.
# - Often requires understanding and knowing which packages are good, which packages
# are fit for purpose

# ---- 2. Import & inspect data ----
# Along with importing the data into the releavant software for linkages
# Inspect the data is important. It allows you to
    # - Ensure data meets expectations (i.e from viewing it via other sources)
    # - Make informed decisions (What needs further attention, what variables are availble)
    # - Understand the structure (Number of records, number of columns)
    # - See the type of consistencies that exist across datasets to be linked

# ---- 3. Clean, Standardise and Wrangle data ----
# Data linkage always requires cleaning. Administrative data is inherently messy, and inconsistent across source. Think of things about how 
# Standardisation is often needed to ensure that when block keys and comparisons are made, that you're comparing apples with apples, otherwise links can easily be missed

# ---- 4. Create Blocks & Link ----
# Blocking is an extremely common part of the linkage process, it's not feasible to simply join datasets
# based on every single combination of rows and then to perform companions ... the number of comparisons rises quadratically (n*(n-1)/2) with the number of records. This is trivial with small datasets, but in DI often we're dealing with larger datasets.
# Linking is the process of which you combine/merge two or more datasets based on common variables (e.g. blocking variables)
# 

# ---- Perform comparisons ----
# One data is linked

# ---- Apply thresholds ----

# ---- Export linkage keys ----