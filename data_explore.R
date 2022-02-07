##########################
# Load and explore data
##########################

#### Grab data from data.dir ####
elab.files <- list.files(data.dir, recursive = TRUE)

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
dem.names <- names(all.counties)[c(5,13:20)]
metrics <- names(all.counties)[c(6:12,21:24)]


#### Function to plot each counties eviction rate by state
evict.rate.state.county <- function(state) {

   state.data <- all.counties[parent.location == state]
   state.years <- state.data[ , unique(year)] %>% sort()
   pdf.path <- paste0(plots.dir, "/", state, "_evict_rate_by_year.pdf")
   state.name <- state.data[ , parent.location] %>% unique()

   # open pdf
   pdf(file = pdf.path, width = 11, height = 8.5)
   message("Plotting data for ", state.name)

   # Run state through plot function
   for (y in seq_along(state.years)) {
      curr.year <- state.years[y]
      temp.plot <- ggplot2::ggplot(state.data[year == curr.year],
                                   aes(x = name, y = eviction.rate)) +
         geom_bar(stat = "identity", width = 0.5) +
         labs(title = (paste0(state.name, " Eviction Rates by County")),
              subtitle = curr.year) +
         xlab("County") + ylab("Eviction Rate (%)") +
         theme(axis.text.x = element_text(angle = 90))

      #plot
      plot(temp.plot)
      message("Plotted: ", state.name, ":", curr.year)
   }
   dev.off()
   message("Done plotting, ", state.name, ", PDF ready for viewing.")
   message("-----------------------------------")
}
