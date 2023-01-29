############################################
# Main script to source files and define global variables
############################################
rm(list = ls(all.names = TRUE))
gc()
graphics.off()

#### check working directory and set if needed; set additional directories ####
if(Sys.info()["sysname"] == "Linux"){
   if(getwd() != normalizePath("~/R/evictionlab-proj")){
      message("Changing working directory to: ", "~/R/evictionlab-proj")
      setwd("~/R/evictionilab-proj")
   } else{
      message("Working directory is: ", getwd())
   }
}

# Load packages and set directories
source("system_init.R")

# Load user-defined functions
fxns <- list.files(fxn.dir)
for (f in fxns %>% seq_along) {
   fct <- paste0(fxn.dir,fxns[f])
   source(fct)
}

# Load in E-Lab data from dir.
source("data_explore.R")

# Load data_completeness script
source("data_completeness.R")

# Load eda_candidates script
source("eda_candidates.R")

# Pull and check data for updates; run as needed.
source("web_scrape.R")


#### Plots ####

##### generate eviction rates by state: year & county with all.counties DT ####
lapply(states.allcounties, evict.rate.state.county) %>% invisible()

##### plot median household income by year with poverty rate labels ####
lapply(states.allcounties, poverty.median.val) %>% invisible()

##### plot rent burden vs poverty rate by county ####
lapply(states.allcounties, prb.func) %>% invisible()


##### plot evictions by demographic variable ####
dem.vars <- names(all.counties)[12:19] %>% sort()
lapply(dem.vars, dem.evict.func) %>% invisible()

##### plot rent burden vs med prop value ####
temp.plot <- ggplot2::ggplot(all.counties,
                             aes(x = rent.burden,
                                 y = median.property.value)) +
   geom_point() +
   labs(title = "Rent Burden vs Median Property Value",
        caption = "All years and counties included.")  +
   scale_x_continuous("Rent Burden (%)") +
   scale_y_continuous("Median Property Value ($)")

#plot
plot(temp.plot)

##### plot rent burden vs median household income ####
temp.plot <- ggplot2::ggplot(all.counties,
                             aes(x = rent.burden,
                                 y = median.household.income)) +
   geom_point() +
   labs(title = "Rent Burden vs Median Household Income",
        caption = "All years and counties included.")  +
   scale_x_continuous("Rent Burden (%)") +
   scale_y_continuous("Median Household Income ($)")

#plot
plot(temp.plot)

##### plot med household income vs eviction rate ####
temp.plot <- ggplot2::ggplot(all.counties,
                             aes(x = median.household.income,
                                 y = eviction.rate)) +
   geom_point() +
   facet_wrap(~year) +
   labs(title = "Median Household Income vs Eviction Rate",
        caption = "All years and counties included.")  +
   scale_x_continuous("Median Household Income ($)") +
   scale_y_continuous("Eviction Rate (%)")


#plot
plot(temp.plot)

##### plot med household income vs evictions ####
temp.plot <- ggplot2::ggplot(all.counties,
                             aes(x = median.household.income,
                                 y = evictions)) +
   geom_point(aes(color = year)) +
   labs(title = "Median Household Income vs Evictions",
        caption = "All years and counties included.")  +
   scale_x_continuous("Median Household Income ($)") +
   scale_y_continuous("Evictions")


#plot
plot(temp.plot)
