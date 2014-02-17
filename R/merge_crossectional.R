# This file is part of the r.eusilc-package (https://github.com/muuankarski/r-eusilc)

# Copyright (C) 2014 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Merge eu-silc personal level cross-sectional raw .csv datasets, personal_register and personal_data
#'
#' @param origin.path A string. Specify the path where to load the original csv files.
#' @param destination.path A string. Specify the path where to save the merged file. value=/code{"not_save"} does not save the merged file
#' @param format A string. Specify the output format for merged data. values=/code{"csv","RData","spss","sas","stata"} 
#' @param subset.vars A string. Specify subset of variables from both datas. /code{"all"} includes all the variables.
#' @param subset.countries A string. Specify subset of countries. In /code{c("FI","SE")} format.  /code{"all"} includes all the countries.
#'
#'
#' @return data.frame
#'
#' @export
#' @examples # per_cross <-  merge_cross_personal(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


merge_cross_personal <- function(origin.path,
                                 destination.path,
                                 format,data.table=FALSE,
                                 subset.vars="all",
                                 subset.countries="all") {
  
  if(!exists("origin.path")) stop("origin.path not defined")
  if(!exists("destination.path")) stop("destination.path not defined")
  if(!exists("format")) stop("format not defined")
  if(!(format %in% c("csv","RData","spss","sas","stata"))) stop("Wrong format. Use csv,RData,spss,sas,stata")
  
  if (data.table == TRUE) {
    suppressPackageStartupMessages(library(data.table))
    suppressPackageStartupMessages(library(bit64))  
  } 
  
  
  # personal register
  path_personal_register <- paste(origin.path,"/r_file.csv",sep="")
  if (data.table == TRUE)  per_reg <- fread(path_personal_register, header = T, sep = ',')
  if (data.table == FALSE) per_reg <- read.csv(path_personal_register, header = T, sep = ',')
  per_reg$PER_ID_Y <- factor(paste(per_reg$RB010,per_reg$RB020,per_reg$RB030, sep="_"))
  per_reg$PER_ID <- factor(paste(per_reg$RB020,per_reg$RB030, sep="_"))
  # personal data
  path_personal_data <- paste(origin.path,"/p_file.csv",sep="")
  if (data.table == TRUE)  per_data <- fread(path_personal_data, header = T, sep = ',')
  if (data.table == FALSE) per_data <- read.csv(path_personal_data, header = T, sep = ',')
  per_data$PER_ID_Y <- factor(paste(per_data$PB010,per_data$PB020,per_data$PB030, sep="_"))
    
  #--------------------------------------------------------------------#
  # merge data
  per_merge_cross <- merge(per_reg,per_data,by="PER_ID_Y", all=TRUE)
  #--------------------------------------------------------------------#
  # subset the merged data
  ## variables
  per_merge_cross <- as.data.frame(per_merge_cross)
  if (subset.vars == "all") {
    per_merge_cross <- per_merge_cross
  } else per_merge_cross <- per_merge_cross[, c(subset.vars)] 
  ## countries
  if (subset.countries == "all") {
    per_merge_cross <- per_merge_cross
  } else  per_merge_cross <- per_merge_cross[per_merge_cross$RB020 %in% c(subset.countries),]
  # write files
  
  if (destination.path != "not_save") {
    if (format == "csv") {
      save_path_personal <- paste(destination.path,"/per_merge_cross.csv",sep="")
      write.csv(per_merge_cross, file=save_path_personal)  
    }
    
    if (format == "RData") {
      save_path_personal <- paste(destination.path,"/per_merge_cross.RData",sep="")
      save(per_merge_cross, file=save_path_personal)
    }
    
    if (format == "spss") {
      library(foreign)
      save_path_personal_datafile <- paste(destination.path,"/per_merge_cross.txt",sep="")
      save_path_personal_codefile <- paste(destination.path,"/per_merge_cross.sps",sep="")
      write.foreign(per_merge_cross,  
                    codefile=save_path_personal_codefile,
                    datafile=save_path_personal_datafile, 
                    package="SPSS") 
    }
    
    if (format == "sas") {
      library(foreign)
      save_path_personal_datafile <- paste(destination.path,"/per_merge_cross.txt",sep="")
      save_path_personal_codefile <- paste(destination.path,"/per_merge_cross.sas",sep="")
      write.foreign(per_merge_cross,  
                    codefile=save_path_personal_codefile,
                    datafile=save_path_personal_datafile, 
                    package="SAS") 
    }
    
    if (format == "stata") {
      library(foreign)
      save_path_personal_datafile <- paste(destination.path,"/per_merge_cross.csv",sep="")
      save_path_personal_codefile <- paste(destination.path,"/per_merge_cross.do",sep="")
      write.foreign(per_merge_cross,  
                    codefile=save_path_personal_codefile,
                    datafile=save_path_personal_datafile, 
                    package="Stata") 
    }
  }
  
  
  rm(list=setdiff(ls(), "per_merge_cross"))
  per_merge_cross
}



#' Merge eu-silc household level longitudinal raw .csv datasets, household_register and household_data
#'
#' @param origin.path A string. Specify the path where to load the original csv files.
#' @param destination.path A string. Specify the path where to save the merged file. value=/code{"not_save"} does not save the merged file
#' @param format A string. Specify the output format for merged data. values=/code{"csv","RData","spss","sas","stata"} 
#' @param subset.vars A string. Specify subset of variables from both datas. /code{"all"} includes all the variables.
#' @param subset.countries A string. Specify subset of countries. In /code{c("FI","SE")} format.  /code{"all"} includes all the countries.
#'
#'
#' @return data.frame
#'
#' @export
#' @examples # hh_cross <-  merge_cross_household(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


merge_cross_household <- function(origin.path,
                                  destination.path,
                                  format,data.table=FALSE,
                                  subset.vars="all",
                                  subset.countries="all") {
  
  if(!exists("origin.path")) stop("origin.path not defined")
  if(!exists("destination.path")) stop("destination.path not defined")
  if(!exists("format")) stop("format not defined")
  if(!(format %in% c("csv","RData","spss","sas","stata"))) stop("Wrong format. Use csv,RData,spss,sas,stata")
  
  if (data.table == TRUE) {
    suppressPackageStartupMessages(library(data.table))
    suppressPackageStartupMessages(library(bit64))  
  }  
  
  # household register
  path_household_register <- paste(origin.path,"/d_file.csv",sep="")
  if (data.table == TRUE)  hh_reg <- fread(path_household_register, header = T, sep = ',')
  if (data.table == FALSE) hh_reg <- read.csv(path_household_register, header = T, sep = ',')
  hh_reg$HH_ID_Y <- factor(paste(hh_reg$DB010,hh_reg$DB020,hh_reg$DB030, sep="_"))
  hh_reg$HH_ID <- factor(paste(hh_reg$DB020,hh_reg$DB030, sep="_"))
  #  household data
  path_household_data <- paste(origin.path,"/h_file.csv",sep="")
  if (data.table == TRUE)  hh_data <- fread(path_household_data, header = T, sep = ',')
  if (data.table == FALSE) hh_data <- read.csv(path_household_data, header = T, sep = ',')
  hh_data$HH_ID_Y <- factor(paste(hh_data$HB010,hh_data$HB020,hh_data$HB030, sep="_"))
  
  #--------------------------------------------------------------------#
  # merge data
  hh_merge_cross <- merge(hh_reg,hh_data,by="HH_ID_Y", all=TRUE)
  # subset the merged data
  ## variables
  hh_merge_cross <- as.data.frame(hh_merge_cross)
  if (subset.vars == "all") {
    hh_merge_cross <- hh_merge_cross
  } else hh_merge_cross <- hh_merge_cross[, c(subset.vars)] 
  ## countries
  if (subset.countries == "all") {
    hh_merge_cross <- hh_merge_cross
  } else  hh_merge_cross <- hh_merge_cross[hh_merge_cross$DB020 %in% c(subset.countries),]
  #--------------------------------------------------------------------#
  
  # write files
  
  if (destination.path != "not_save") {
    if (format == "csv") {
      save_path_household <- paste(destination.path,"/hh_merge_cross.csv",sep="")
      write.csv(hh_merge_cross, file=save_path_household)  
    }
    
    if (format == "RData") {
      save_path_household <- paste(destination.path,"/hh_merge_cross.RData",sep="")
      save(hh_merge_cross, file=save_path_household)
    }
    
    if (format == "spss") {
      library(foreign)
      save_path_household_datafile <- paste(destination.path,"/hh_merge_cross.txt",sep="")
      save_path_household_codefile <- paste(destination.path,"/hh_merge_cross.sps",sep="")
      write.foreign(hh_merge_cross,  
                    codefile=save_path_household_codefile,
                    datafile=save_path_household_datafile, 
                    package="SPSS") 
    }
    
    if (format == "sas") {
      library(foreign)
      save_path_household_datafile <- paste(destination.path,"/hh_merge_cross.txt",sep="")
      save_path_household_codefile <- paste(destination.path,"/hh_merge_cross.sas",sep="")
      write.foreign(hh_merge_cross,  
                    codefile=save_path_household_codefile,
                    datafile=save_path_household_datafile, 
                    package="SAS") 
    }
    
    if (format == "stata") {
      library(foreign)
      save_path_household_datafile <- paste(destination.path,"/hh_merge_cross.csv",sep="")
      save_path_household_codefile <- paste(destination.path,"/hh_merge_cross.do",sep="")
      write.foreign(hh_merge_cross,  
                    codefile=save_path_household_codefile,
                    datafile=save_path_household_datafile, 
                    package="Stata") 
    }
  }
  
  
  rm(list=setdiff(ls(), "hh_merge_cross"))
  hh_merge_cross
}
