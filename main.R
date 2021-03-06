############################################
# Main script to source files and define global variables
############################################
rm(list = ls())
graphics.off()

# Load packages and set directories
source("system_init.R")

# Load users functions
source("functions.R")

# Pull and check data for updates
# run as needed.
##  source("web_scrape.R")

# Load in E-Lab data from dir.
source("data_explore.R")


#### Plots ####

# generate eviction rates by state: year & county with all.counties DT
lapply(states.allcounties, evict.rate.state.county) %>% invisible()


# plot median household income by year with poverty rate labels
lapply(states.allcounties, poverty.median.val) %>% invisible()

