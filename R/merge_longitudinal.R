# This file is part of the r.eusilc-package (https://github.com/muuankarski/r-eusilc)

# Copyright (C) 2014 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Merge eu-silc cross-sectional and longitudinal raw .csv datasets
#'
#' @param origin.path A string. Specify the path where to load the original csv files.
#' @param destination.path A string. Specify the path where to save the merged file.
#'
#' @return data.frame
#'
#' @export
#' @examples # merge_longitudinal(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged")
#' @author Markus Kainu <markuskainu(at)gmail.com> 

merge_longitudinal <- function(origin.path,destination.path) {

# household register 
hh_reg <- read.csv("origin.path/d_file.csv", header=TRUE)
hh_reg$HH_ID_Y <- factor(paste(hh_reg$DB010,hh_reg$DB020,hh_reg$DB030, sep="_"))
hh_reg$HH_ID <- factor(paste(hh_reg$DB020,hh_reg$DB030, sep="_"))
# household data
hh_data <- read.csv("origin.path/h_file.csv", header=TRUE)
hh_data$HH_ID_Y <- factor(paste(hh_data$HB010,hh_data$HB020,hh_data$HB030, sep="_"))
hh_data$HH_ID <- factor(paste(hh_data$HB020,hh_data$HB030, sep="_"))
# personal register
per_reg <- read.csv("origin.path/r_file.csv", header=TRUE)
per_reg$PER_ID_Y <- factor(paste(per_reg$RB010,per_reg$RB020,per_reg$RB030, sep="_"))
per_reg$PER_ID <- factor(paste(per_reg$RB020,per_reg$RB030, sep="_"))
# personal data
per_data <- read.csv("origin.path/p_file.csv", header=TRUE)
per_data$PER_ID_Y <- factor(paste(per_data$PB010,per_data$PB020,per_data$PB030, sep="_"))
per_data$PER_ID <- factor(paste(per_data$PB020,per_data$PB030, sep="_"))
#--------------------------------------------------------------------#
# merge data
# merge household datas
library(plyr)
hh_merge <- join(hh_reg,hh_data,by="HH_ID_Y", type="full")
# merge personal datas
per_merge <- join(per_reg,per_data,by="PER_ID_Y", type="full")
#per_merge$HH_ID_Y <- factor(paste(per_merge$RB010,per_merge$RB020,per_merge$PX030, sep="_"))
# merge household and personal into single data
#merge <- join(hh_merge,per_merge,by="HH_ID_Y", type="full")
#--------------------------------------------------------------------#
# Write merged file separate .RData files
save(hh_merge, file="destination.path/hh_merge.RData")
save(per_merge, file="destination.path/per_merge.RData")
rm()

}