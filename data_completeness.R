############################################
##### Data Completeness Exploration ########
############################################

### Completeness Metrics ###

# Generate tables of metadata of each state according to block.groups, tracts, and counties----------------------------------------------------------
census.levels <- c("block.groups", "tracts", "counties")
for (t in census.levels %>% seq_along()) {
   name <- census.levels[t]
   tbl <- try(get_metadata(name))
   if()
   assign(paste0(name,".metadata"), tbl)
}
metadata.all <- rbind(block.groups.metadata,
                      tracts.metadata,
                      counties.metadata)

# Write to Google -------------------------------------------------------------------------------------------------------------------------------
if(metadata.all %>% nrow() == 132){
   TDA.file <- as_sheets_id("https://docs.google.com/spreadsheets/d/1o1YUPIleg0LeakjJ9AePPJlx2c6nA_mEXkQxLv5nXsg/edit#gid=0")
   googlesheets4::write_sheet(metadata.all,
                              ss = TDA.file,
                              sheet = "Metadata")
}


