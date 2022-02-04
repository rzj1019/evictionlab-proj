#############################
# Functions for Eviction Lab R project
#############################

# get diff of column names between 2 tables
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


# grab key object from aws.s3 table and put into elab.keylist
keygrabber <- function(){

}