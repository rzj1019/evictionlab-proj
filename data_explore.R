##########################
# Load and explore data
##########################

#### Grab data from data.dir ####
elab.files <- list.files(data.dir, recursive = TRUE)
validated.elab.files <- elab.files[!grepl("legacy-data",elab.files)]
# Make table for each elab file and put in state list if needed
for(f in elab.files %>% seq_along()) {
   name <- stringr::str_extract(elab.files[f], "[^/]+(?=\\.)") %>% as.character()
   name <- stringr::str_replace_all(name, "[-_]", "." )
   table <- readRDS(paste0(data.dir, elab.files[f])) %>% as.data.table()
   assign(name, table)
   message("Created table: ", name)
}



### Set variables needed####
# Check column names for each type of file
col.names <- c("GEOID", "year", "name", "parent.location", "population", "poverty.rate", "renter.occupied.households",
                 "pct.renter.occupied", "median.gross.rent", "median.household.income", "median.property.value",
                 "rent.burden", "pct.white", "pct.af.am", "pct.hispanic", "pct.am.ind", "pct.asian", "pct.nh.pi",
                 "pct.multiple", "pct.other", "eviction.filings", "evictions", "eviction.rate", "eviction.filing.rate",
                 "low.flag", "imputed", "subbed")

years.allcounties <- all.counties[ , year] %>% unique()
counties.allcounties <- all.counties[ , name] %>% unique()
states.allcounties <- all.counties[ , parent.location] %>% unique()
demo.names <- names(all.counties)[c(5,13:20)]
metrics <- names(all.counties)[c(6:12,21:24)]

