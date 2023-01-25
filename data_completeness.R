############################################
##### Data Completeness Exploration ########
############################################

### Completeness Metrics ###

# Generate tables of metadata of each state according to block.groups, tracts, and counties----------------------------------------------------------
census.levels <- c("block.groups", "tracts", "counties")
block.groups.metadata <- eviction_metadata(census.levels[1])
tracts.metadata <- eviction_metadata(census.levels[2])
counties.metadata <- eviction_metadata(census.levels[3])
metadata.all <- rbind(block.groups.metadata,
                      tracts.metadata,
                      counties.metadata)

# Write to Google -------------------------------------------------------------------------------------------------------------------------------
TDA.file <- as_sheets_id("https://docs.google.com/spreadsheets/d/1o1YUPIleg0LeakjJ9AePPJlx2c6nA_mEXkQxLv5nXsg/edit#gid=0")
googlesheets4::write_sheet(metadata.all,
                           ss = TDA.file,
                           sheet = "Metadata")

