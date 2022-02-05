#############################
# Functions for Eviction Lab R project
#############################

#### Get diff of Column Names Between 2 Tables ####
namesdiff <- function(x, y) {
   message("Columns in ", deparse(substitute(x)), " that are not in ", deparse(substitute(y)), ":")
   xdiff <- dplyr::setdiff(names(x), names(y))
   for (x in 1:length(xdiff)) {
      cat(xdiff[x], "\n")
   }
   message("Columns ", deparse(substitute(y)), " that are not in ", deparse(substitute(x)), ":")
   ydiff <- dplyr::setdiff(names(y), names(x))
   for (y in 1:length(ydiff)) {
      cat(ydiff[x], "\n")
   }
}


#### Save Local if New, Check if Update from AWS ###
loadncheck.bucket <- function(key.name) {
   name <- stringr::str_remove(key.name, "\\.csv*")
   dir.check <- str_split(name, "/", simplify = TRUE)
   dir.length <- length(dir.check)
   file.path <- paste0(data.dir, name)

   # Check for multiple directories
   if(dir.length > 2) {
      dir.grep <- stringr::str_flatten(rep(".+/", times = length(dir.check)-1))
      dir.name <- stringr::str_extract(name, dir.grep)
      file.name <- stringr::str_extract(name, "/.+")
   } else {
      dir.name <- stringr::str_extract(name, ".+/")
      file.name <- stringr::str_extract(name, "/.+")
      }

   # Check file extension
   if(stringr::str_ends(key.name, ".csv")){

      # 1.) Check if file exists
      # 2.) Check if data is updated and update accordingly
      if(!file.exists(paste0(file.path,".rds"))) {

         # 1.-----------------------------
         if(!file.exists(paste0(data.dir, dir.name))) {
            dir.create(paste0(data.dir, dir.name), recursive = TRUE)
            message("Created directory: ", dir.name)
         }
         message("Reading: ", file.name)
         key.rds <- aws.s3::s3read_using(FUN = fread,
                                         bucket = bucketname,
                                         object = key.name)
         message("Creating RDS: ", file.name)
         saveRDS(key.rds, file = paste0(file.path, ".rds"))
         message("Saved: ", file.name)
         message("-------------------------------------")
      } else {
         # 2.-----------------------------------------------
         message("Comparing new file to old: ", file.name)
         key.rds.new <- aws.s3::s3read_using(FUN = fread,
                                             bucket = bucketname,
                                             object = key.name)
         key.rds <- readRDS(file = paste0(file.path, ".rds"))
         if(nrow(setdiff(key.rds.new, key.rds))>0){
            saveRDS(key.rds.new, file = paste0(file.path, ".rds"))
            message(file.name, " data has changed, updated to new data")
         }else{
            message(file.name, " data has not updated.")
            message("-------------------------------------")
         }
      }

   } else{
      message(name, " not csv, moving to next file.")
      message("---------------------------------------")
   }

   return(name)
}
