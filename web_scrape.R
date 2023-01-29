##########################
# Web-scrapeEvictionlab.org and connec to AWS API
#########################

#### AWS API:eviction-lab-data-downloads #####
bucketname <- "eviction-lab-data-downloads"
bucketdata <- aws.s3::get_bucket(bucketname)
webpath <- paste0("s3://",bucketname, "/")
keynames<- c()

# loop to grab key list
for (i in bucketdata %>% seq_along()) {
   key <- c(bucketdata[[i]][[1]])
   keynames <- c(keynames,key)
}

# Run data pull and check
lapply(keynames, loadncheck_bucket)
