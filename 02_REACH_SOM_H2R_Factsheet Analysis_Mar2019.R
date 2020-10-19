# Global variables
# 
# ## pilot
# # files we read
# AGGREGATED_SETTLEMENT_DATASET <- "./01jan-feb/output/REACH_SOM_H2R_Settlements_Merged_Mar2019.csv"
# #files we write
# DATA_OUTPUT <- "REACH_SOM_H2R_Factsheet_Analysis_Mar2019.csv"
# output_folder <- "./01jan-feb/output/"

# ## march
# # files we read
# AGGREGATED_SETTLEMENT_DATASET <- "./02march/output/REACH_SOM_H2R_Settlements_Merged_Apil2019.csv"
# #files we write
# DATA_OUTPUT <- "REACH_SOM_H2R_Factsheet_Analysis_april2019.csv"
# output_folder <- "./02march/output/"

## march - b
# files we read 

AGGREGATED_SETTLEMENT_DATASET <-  "./02march-b/output/REACH_SOM_H2R_Settlements_Merged_April2019_baidoa.csv"
#files we write
DATA_OUTPUT <- "REACH_SOM_H2R_Factsheet_Analysis_april2019_b.csv"
output_folder <- "./02march-b/output/"


# library imports and working directory
library(tidyverse)
# setwd("D:/R_Scripts/SOM_H2R")
setwd("C:/Users/Yann/Desktop/h2r")


# Reading our data

d.f <- read.csv(AGGREGATED_SETTLEMENT_DATASET, stringsAsFactors = FALSE, colClasses = "character")

hasdata <- function(x) {
  x[which(!is.null(x) & !is.na(x) & x !="")]  
}

cbindPad <- function(...) {
  args <- list(...) ##I need to define again the list as a list because do.whatever functions turn list into anything, therefore I need to redefine.
  n <- sapply(args, nrow)
  mx <- max(n) ##getting the maximun number of rows of the longest table
  pad <- function(x) {
    if (nrow(x) < mx) {
      padTemp <- matrix(NA, mx - nrow(x), ncol(x)) ##creating a matrix of NA
      colnames(padTemp) <- colnames(x)
      if (ncol(x) == 0) {
        return(padTemp)
      }
      else {
        return(rbind(x, padTemp))
      }
    }
    else {
      return(x)
    }
  }
  rs <- lapply(args, pad)
  return(do.call(cbind, rs))
}

sumSort <- function(x) {
  x <- x[x != "SL"]
  if (length(x) > 0 ) {
    d.f2 <- as.data.frame(prop.table(table(hasdata(x))))
    return(d.f2[order(d.f2[,2], decreasing = TRUE),])
  } else {
    d.f2 <- data.frame(matrix(c("No data", "No data"), nrow = 1, ncol = 2))
    names(d.f2) <- c("Var1", "Freq")
    return(d.f2)
  }
}

# Analysis output


d.f %>%
  select(-month, -ki_coverage) %>%
  lapply(sumSort) %>%
  do.call(what = cbindPad) %>%
  write.csv(file = paste0(output_folder, DATA_OUTPUT), na = "")

write_output <- function(dataset, region = NULL) {
    dataset %>%
        select(-month, -ki_coverage) %>%
        lapply(sumSort) %>%
        do.call(what = cbindPad) %>%
        write.csv(file = paste0(output_folder,region, "_", DATA_OUTPUT), na = "")
}

variable_to_split_log_by <- "info_reg"
each_group <- d.f[[variable_to_split_log_by]]
list_df <- d.f %>% split.data.frame(list(each_group))
names_df <- names(list_df)

mapply(write_output, dataset = list_df, region = names_df)
