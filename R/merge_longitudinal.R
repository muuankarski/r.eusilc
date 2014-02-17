# This file is part of the r.eusilc-package (https://github.com/muuankarski/r-eusilc)

# Copyright (C) 2014 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Merge eu-silc personal level longitudinal raw .csv datasets, personal_register and personal_data
#'
#' @param origin.path A string. Specify the path where to load the original csv files.
#' @param destination.path A string. Specify the path where to save the merged file. value=/code{"not_save"} does not save the merged file
#'
#' @return data.frame
#'
#' @export
#' @examples # per_longi <-  merge_longi_personal(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


merge_longi_personal <- function(origin.path,destination.path,format) {
  
  if(!exists("origin.path")) stop("origin.path not defined")
  if(!exists("destination.path")) stop("destination.path not defined")
  if(!exists("format")) stop("format not defined")
  if(!(format %in% c("csv","RData","spss","sas","stata"))) stop("Wrong format. Use csv,RData,spss,sas,stata")
  
  # suppressPackageStartupMessages(library(data.table))
  # suppressPackageStartupMessages(library(bit64))
  
  
# personal register
path_personal_register <- paste(origin.path,"/r_file.csv",sep="")
# per_reg <- fread(path_personal_register, header = T, sep = ',')
per_reg <- read.csv(path_personal_register, header = T, sep = ',')
per_reg$PER_ID_Y <- factor(paste(per_reg$RB010,per_reg$RB020,per_reg$RB030, sep="_"))
per_reg$PER_ID <- factor(paste(per_reg$RB020,per_reg$RB030, sep="_"))
# personal data
path_personal_data <- paste(origin.path,"/p_file.csv",sep="")
# per_data <- fread(path_personal_data, header = T, sep = ',')
per_data <- read.csv(path_personal_data, header = T, sep = ',')
per_data$PER_ID_Y <- factor(paste(per_data$PB010,per_data$PB020,per_data$PB030, sep="_"))
per_data$PER_ID <- factor(paste(per_data$PB020,per_data$PB030, sep="_"))

#--------------------------------------------------------------------#
# merge data
per_merge_longi <- merge(per_reg,per_data,by="PER_ID_Y", all=TRUE)
#--------------------------------------------------------------------#
# write files

if (destination.path != "not_save") {
  if (format == "csv") {
    save_path <- paste(destination.path,"/per_merge_longi.csv",sep="")
    write.csv(per_merge_longi, file=save_path)  
  }
  
  if (format == "RData") {
    save_path <- paste(destination.path,"/per_merge_longi.RData",sep="")
    save(per_merge_longi, file=save_path)
  }
  
  if (format == "spss") {
    library(foreign)
    save_path_datafile <- paste(destination.path,"/per_merge_longi.txt",sep="")
    save_path_codefile <- paste(destination.path,"/per_merge_longi.sps",sep="")
    write.foreign(per_merge_longi,  
                  codefile=save_path_codefile,
                  datafile=save_path_datafile, 
                  package="SPSS") 
  }
  
  if (format == "sas") {
    library(foreign)
    save_path_datafile <- paste(destination.path,"/per_merge_longi.txt",sep="")
    save_path_codefile <- paste(destination.path,"/per_merge_longi.sas",sep="")
    write.foreign(per_merge_longi,  
                  codefile=save_path_codefile,
                  datafile=save_path_datafile, 
                  package="SAS") 
  }
  
  if (format == "stata") {
    library(foreign)
    save_path_datafile <- paste(destination.path,"/per_merge_longi.csv",sep="")
    save_path_codefile <- paste(destination.path,"/per_merge_longi.do",sep="")
    write.foreign(per_merge_longi,  
                  codefile=save_path_codefile,
                  datafile=save_path_datafile, 
                  package="Stata") 
  }
}


rm(list=setdiff(ls(), "per_merge_longi"))
per_merge_longi
}



#' Merge eu-silc household level longitudinal raw .csv datasets, household_register and household_data
#'
#' @param origin.path A string. Specify the path where to load the original csv files.
#' @param destination.path A string. Specify the path where to save the merged file. value=/code{"not_save"} does not save the merged file
#'
#' @return data.frame
#'
#' @export
#' @examples # hh_longi <-  merge_longi_household(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


merge_longi_household <- function(origin.path,destination.path,format) {
  
  if(!exists("origin.path")) stop("origin.path not defined")
  if(!exists("destination.path")) stop("destination.path not defined")
  if(!exists("format")) stop("format not defined")
  if(!(format %in% c("csv","RData","spss","sas","stata"))) stop("Wrong format. Use csv,RData,spss,sas,stata")
  
  # suppressPackageStartupMessages(library(data.table))
  # suppressPackageStartupMessages(library(bit64))
  
  
  # household register
  path_household_register <- paste(origin.path,"/d_file.csv",sep="")
  # hh_reg <- fread(path_household_register, header = T, sep = ',')
  hh_reg <- read.csv(path_household_register, header = T, sep = ',')
  hh_reg$HH_ID_Y <- factor(paste(hh_reg$DB010,hh_reg$DB020,hh_reg$DB030, sep="_"))
  hh_reg$HH_ID <- factor(paste(hh_reg$DB020,hh_reg$DB030, sep="_"))
  # personal data
  path_household_data <- paste(origin.path,"/h_file.csv",sep="")
  # hh_data <- fread(path_household_data, header = T, sep = ',')
  hh_data <- read.csv(path_household_data, header = T, sep = ',')
  hh_data$HH_ID_Y <- factor(paste(hh_data$HB010,hh_data$HB020,hh_data$HB030, sep="_"))
  hh_data$HH_ID <- factor(paste(hh_data$HB020,hh_data$HB030, sep="_"))
  
    
  #--------------------------------------------------------------------#
  # merge data
  hh_merge_longi <- merge(hh_reg,hh_data,by="HH_ID_Y", all=TRUE)
  #--------------------------------------------------------------------#
  # write files
  
  if (destination.path != "not_save") {
    if (format == "csv") {
      save_path <- paste(destination.path,"/hh_merge_longi.csv",sep="")
      write.csv(hh_merge_longi, file=save_path)  
    }
    
    if (format == "RData") {
      save_path <- paste(destination.path,"/hh_merge_longi.RData",sep="")
      save(hh_merge_longi, file=save_path)
    }
    
    if (format == "spss") {
      library(foreign)
      save_path_datafile <- paste(destination.path,"/hh_merge_longi.txt",sep="")
      save_path_codefile <- paste(destination.path,"/hh_merge_longi.sps",sep="")
      write.foreign(hh_merge_longi,  
                    codefile=save_path_codefile,
                    datafile=save_path_datafile, 
                    package="SPSS") 
    }
    
    if (format == "sas") {
      library(foreign)
      save_path_datafile <- paste(destination.path,"/hh_merge_longi.txt",sep="")
      save_path_codefile <- paste(destination.path,"/hh_merge_longi.sas",sep="")
      write.foreign(hh_merge_longi,  
                    codefile=save_path_codefile,
                    datafile=save_path_datafile, 
                    package="SAS") 
    }
    
    if (format == "stata") {
      library(foreign)
      save_path_datafile <- paste(destination.path,"/hh_merge_longi.csv",sep="")
      save_path_codefile <- paste(destination.path,"/hh_merge_longi.do",sep="")
      write.foreign(hh_merge_longi,  
                    codefile=save_path_codefile,
                    datafile=save_path_datafile, 
                    package="Stata") 
    }
  }
  
  
  rm(list=setdiff(ls(), "hh_merge_longi"))
  hh_merge_longi
}

