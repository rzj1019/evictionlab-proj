############################
# Environment loader
###########################

rm(list = ls())
graphics.off()


#### check working directory and set if needed; set additional directories ####
if(Sys.info()[1] == "Linux"){
  if(getwd() == "~/R/evictionlab-proj"){
    message("Working directory not set appropriately. Changing working directory to: ")
    message(getwd())
  } else{
    setwd("~/R/evictionlab-proj")
    message("Working directory is: ", getwd())

  }
}

# set directories
dir <- getwd()
data.dir <- paste0(dir, "/data/")

##### Loading Needed packages ####
# Require and install pacman for loading all needed packages
if (!require("pacman", character.only = TRUE)) {
   install.packages("pacman")
   library(pacman)
}

# List of packages to be loaded
pkgs <- c("assertable",
          "aws.s3",
          "data.table",
          "installr",
          "readxl",
          "rvest",
          "shiny",
          "tidyverse")

pacman::p_load(pkgs, character.only = TRUE)
pacman::p_loaded()

### AWS Login ###
Sys.setenv(
   "AWS_ACESS_KEY_ID" = "<your access key",
   "AWS_SECRET_ACCESS_KEY" = "<your secret key>",
   "AWW_DEFAULT_REGION" = "your region"
)
