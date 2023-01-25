###############################################
########### EDA for TDA Candidates ############
###############################################

# Exploratory Data Analysis for TDA Candidates

# List of top 5 candidates
metadata.all[level == "counties"][order(-pct.level.coverage,-pct.row.coverage)] %>% head()

# Like WI and IA b/c row coverage > .70
candidates <- c("WI", "IA")

wi.metadata <- metadata.all[state.abb == "WI"]
ia.metadata <- metadata.all[state.abb == "IA"]

# Finding candidate missing years for each county; returns (state).missing.years
wi.missing.years <- missing_data("WI")
ia.missing.years <- missing_data("IA")


# EDA

# see table if NAs are dropped
metadata.all.na <- na.omit(metadata.all)

# get numeric WI.counties
wi.counties.numeric <-WI.counties %>% select_if(is.numeric)

# mean
wi.counties.numeric[, lapply(.SD, mean)]

# sd
wi.counties.numeric[, lapply(.SD, sd)]
