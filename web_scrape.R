##########################
# Web-scrapeEvictionlab.org and connec to AWS API
#########################

#### Web Scraping ####
url <- read_html("https://data-downloads.evictionlab.org/")
footer <- url %>% html_node(".app-footer") %>% html_text()
mapanddata <- str_locate(footer, "Map and Data")


#### AWS API:eviction-lab-data-downloads #####

bucketname <- "eviction-lab-data-downloads"
bucketdata <- aws.s3::get_bucket(bucketname)
webpath <- paste0("s3://",bucketname, "/")
keynames<- data.table(keys = as.character())

# loop to grab key list
for (i in 1:length(bucketdata)) {
   key <- c(bucketdata[[i]][[1]])
   keynames <- rbind(keynames, key, use.names = FALSE)
}

