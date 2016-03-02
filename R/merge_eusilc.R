# This file is part of the r.eusilc-package (https://github.com/muuankarski/r-eusilc)

# Copyright (C) 2014-2016 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Merge eu-silc personal level cross-sectional raw .csv datasets, personal_register and personal_data
#'
#' @param path.personal.register A string. Specify the path to original personal register .csv file.
#' @param path.personal.data A string. Specify the path to original personal data .csv file.
#' @param path.household.register A string. Specify the path to original household register .csv file.
#' @param path.household.data A string. Specify the path to original household data .csv file.
#' @param destination.path A string. Specify the path where to save the merged file. value=\code{"not_save"} does not save the merged file
#' @param level A string. Specify the whether to merge \code{"personal"}, \code{"household"} or \code{"both"} level datas. Both stands for merging all the four datas at personal level
#' @param type A string. Specify the whether to merge \code{"cross-sectional"} or \code{"longitudinal"} level datas
#' @param year A string. Specify the year from what year data is in question
#' @param format A string. Specify the output format for merged data. values=\code{"csv","RData","SPSS","SAS" or "Stata"} 
#' @param subset.vars.per.reg. A string. Specify subset of variables from personal register file. \code{"all"} includes all the variables.
#' @param subset.vars.per.data. A string. Specify subset of variables from personal data file. \code{"all"} includes all the variables.
#' @param subset.vars.hh.reg. A string. Specify subset of variables from household register file. \code{"all"} includes all the variables.
#' @param subset.vars.hh.data. A string. Specify subset of variables from household data file. \code{"all"} includes all the variables.
#' @param subset.countries A string. Specify subset of countries. In \code{c("FI","SE")} format.  \code{"all"} includes all the countries.
#'
#'
#' @return data.frame
#'
#' @export
#' @examples # 
#' @author Markus Kainu <markuskainu(at)gmail.com> 


merge_eusilc <- function(path.personal.register,
                         path.personal.data,
                         path.household.register,
                         path.household.data,
                         output.path,
                         output.filename = "default",
                         return.object = TRUE,
                         level,
                         type,
                         year,
                         format,
                         subset.vars.per.reg="all",
                         subset.vars.per.data="all",
                         subset.vars.hh.reg="all",
                         subset.vars.hh.data="all",
                         subset.countries="all") {
  
  if (level == "personal") {
    if(!exists("path.personal.register")) stop("path.personal.register not defined")
    if(!exists("path.personal.data")) stop("path.personal.data not defined")
  }
  if (level == "household") {
    if(!exists("path.household.register")) stop("path.household.register not defined")
    if(!exists("path.household.data")) stop("path.household.data not defined")
  }
  if (level == "both") {
    if(!exists("path.personal.register")) stop("path.personal.register not defined")
    if(!exists("path.personal.data")) stop("path.personal.data not defined")
    if(!exists("path.household.register")) stop("path.household.register not defined")
    if(!exists("path.household.data")) stop("path.household.data not defined")
  }
  if(!exists("output.path")) stop("output.path not defined")
  if(!exists("level")) stop("level not defined")
  if(!exists("type")) stop("type not defined")
  if(!exists("year")) stop("year not defined")
  if(!exists("format")) stop("format not defined")
  if(!(format %in% c("csv","RData","SPSS","SAS","Stata"))) stop("Wrong format. Use csv,RData,SPSS,SAS,Stata")
   
  # ----------------------------------------------- #
  # ----------------------------------------------- #
  #   function to subset data
  
subset.vars.per <- function(data) {
  # personal data
    if (data == "per.data") {
    if (subset.vars.per.data[1] == "all") subset.vars.per.data <- names(per.data)
    if (subset.vars.per.data[1] != "all") subset.vars.per.data <- subset.vars.per.data
    subset.vars.per.data <- append(subset.vars.per.data, 
                              #c("PER_ID_Y","PX030","PB020"), 
                              c("PB010","PB020","PB030","PX030"), 
                              after=0)
    subset.vars.per.data <- unique(subset.vars.per.data)
    per.data <- per.data[, subset.vars.per.data]
    # countries
    if (subset.countries[1] == "all") per.data <- per.data
    if (subset.countries[1] != "all") per.data <- per.data[per.data$PB020 %in% subset.countries,]
    return(per.data)
    }
    # ---------------------------------------- #
    if (data == "per.reg") {
    # personal register
    if (subset.vars.per.reg[1] == "all") subset.vars.per.reg <- names(per.reg)
    if (subset.vars.per.reg[1] != "all") subset.vars.per.reg <- subset.vars.per.reg
    subset.vars.per.reg <- append(subset.vars.per.reg, 
                                  #c("PER_ID","PER_ID_Y","RB010","RB020"), 
                                  c("RB010","RB020","RB030"), 
                                  after=0)
    subset.vars.per.reg <- unique(subset.vars.per.reg)
    
    per.reg <- per.reg[, subset.vars.per.reg]
    # countries
    if (subset.countries[1] == "all") per.reg <- per.reg
    if (subset.countries[1] != "all") per.reg <- per.reg[per.reg$RB020 %in% subset.countries,]
    return(per.reg)
    }
  }
  # ----------------------------------------------- #

subset.vars.hh <- function(data) {
    if (data == "hh.data") {
        if (subset.vars.hh.data[1] == "all") subset.vars.hh.data <- names(hh.data)
        if (subset.vars.hh.data[1] != "all") subset.vars.hh.data <- subset.vars.hh.data
        subset.vars.hh.data <- append(subset.vars.hh.data, 
                                       c("HB010","HB020","HB030"), 
                                       after=0)
        subset.vars.hh.data <- unique(subset.vars.hh.data)
        hh.data <- hh.data[, subset.vars.hh.data]
        # countries
        if (subset.countries[1] == "all") hh.data <- hh.data
        if (subset.countries[1] != "all") hh.data <- hh.data[hh.data$HB020 %in% subset.countries,]
        return(hh.data)
    }

    # ---------------------------------------- #
    if (data == "hh.reg") {
        # hhsonal register
        if (subset.vars.hh.reg[1] == "all") subset.vars.hh.reg <- names(hh.reg)
        if (subset.vars.hh.reg[1] != "all") subset.vars.hh.reg <- subset.vars.hh.reg
        subset.vars.hh.reg <- append(subset.vars.hh.reg, 
                                     c("DB010","DB020","DB030"),
                                      after=0)
        subset.vars.hh.reg <- unique(subset.vars.hh.reg)
        
        hh.reg <- hh.reg[, subset.vars.hh.reg]
        # countries
        if (subset.countries[1] == "all") hh.reg <- hh.reg
        if (subset.countries[1] != "all") hh.reg <- hh.reg[hh.reg$DB020 %in% subset.countries,]
        return(hh.reg)
    }
}

  # ----------------------------------------------- #
  # ----------------------------------------------- #  
  #   loading and merge the raw data

  # Personal
  if (level == "personal") {
    ## personal register
    path_personal_register <- path.personal.register
    per.reg <- read_csv(path_personal_register)
    
    ## personal data
    path_personal_data <- path.personal.data
    per.data <- read_csv(path_personal_data)
    
    # subset the data before merging
    per.reg <- subset.vars.per("per.reg")
    per.data <- subset.vars.per("per.data")
  
    # merge personal register with personal data
    merged <- merge(per.reg,per.data,
                    by.x=c("RB010","RB020","RB030"),
                    by.y=c("PB010","PB020","PB030"),
                    all=TRUE)
    rm(list = c('per.reg','per.data')) # to spare memory
  }

  
  # Household
  if (level == "household") {
    ## household register
    path_household_register <- path.household.register
    hh.reg <- read_csv(path_household_register)
    
    ## household data
    path_household_data <- path.household.data
    hh.data <- read_csv(path_household_data)
    
    # subset the data before merging
    hh.reg <- subset.vars.hh("hh.reg")
    hh.data <- subset.vars.hh("hh.data")

    # merge household register with household data
    merged <- merge(hh.reg,hh.data,
    	by.x=c("DB010","DB020","DB030"),
    	by.y=c("HB010","HB020","HB030"), all=TRUE)
    
    rm(list = c('hh.reg','hh.data')) # to spare memory
  }
  
  # Both
  if (level == "both") {
    ## personal register
    path_personal_register <- path.personal.register
    per.reg <- read_csv(path_personal_register)
    
    ## personal data
    path_personal_data <- path.personal.data
    per.data <- read_csv(path_personal_data)
        
    # subset the data before merging
    per.reg <- subset.vars.per("per.reg")
    per.data <- subset.vars.per("per.data")
  
    # merge personal register with personal data
    per.merged <- merge(per.reg,per.data,
                    by.x=c("RB010","RB020","RB030"),
                    by.y=c("PB010","PB020","PB030"),
                    all=TRUE)
    # per.merged$HH_ID_Y <- factor(paste(per.merged$RB010,
    #                                    per.merged$RB020,
    #                                    per.merged$PX030, 
    #                                    sep="_"))

    ## household register
    path_household_register <- path.household.register
    hh.reg <- read_csv(path_household_register)
    
    ## household data
    path_household_data <- path.household.data
    hh.data <- read_csv(path_household_data)
    
    # subset the data before merging
    hh.reg <- subset.vars.hh("hh.reg")
    hh.data <- subset.vars.hh("hh.data")

    # merge household register with household data
    hh.merged <- merge(hh.reg,hh.data,
    	by.x=c("DB010","DB020","DB030"),
    	by.y=c("HB010","HB020","HB030"), all=TRUE)
    
    rm(list = c('hh.reg','hh.data')) # to spare memory

    merged <- merge(per.merged,hh.merged,
    	by.x=c("RB010","RB020","PX030"),
    	by.y=c("DB010","DB020","DB030"), all=TRUE)
    
    rm(list = c('per.reg','per.data')) # to spare memory
      
      #by="HH_ID_Y",all=TRUE)
    
    } 

  # ----------------------------------------------- #
  # ----------------------------------------------- #

  # write files
  
    save_path <- paste(output.path,"/",output.filename,sep="")
  
  if (output.path != "not_save") {
    if (format == "csv") {
      save_path_csv <- paste(save_path,".csv",sep="")
      write.csv(merged, file=save_path_csv)  
    }
    
    if (format == "RData") {
      save_path_rdata <- paste(save_path,".RData",sep="")
      if (level == "personal" & type == "cross-sectional") {
        # per_merge_cross <- merged
        # save(per_merge_cross, file=save_path_rdata)
        save(merged, file=save_path_rdata)
      }
      if (level == "household" & type == "cross-sectional") {
        # hh_merge_cross <- merged
        save(merged, file=save_path_rdata)
      }
      if (level == "personal" & type == "longitudinal") {
        # per_merge_longi <- merged
        save(merged, file=save_path_rdata)
      }
      if (level == "household" & type == "longitudinal") {
        # hh_merge_longi <- merged
        save(merged, file=save_path_rdata)
      }
      if (level == "both" & type == "cross-sectional") {
        # both_merge_cross <- merged
        save(merged, file=save_path_rdata)
      }
      if (level == "both" & type == "longitudinal") {
        # both_merge_longi <- merged
        save(merged, file=save_path_rdata)
      }
    }
    
    if (format == "SPSS") {
      library(foreign)
      save_path_datafile <- paste(save_path,".txt",sep="")
      save_path_codefile <- paste(save_path,".sps",sep="")
      write.foreign(merged,  
                    codefile=save_path_codefile,
                    datafile=save_path_datafile, 
                    package="SPSS") 
    }
    
    if (format == "SAS") {
      library(foreign)
      save_path_datafile <- paste(save_path,".txt",sep="")
      save_path_codefile <- paste(save_path,".sas",sep="")
      write.foreign(merged,  
                    codefile=save_path_codefile,
                    datafile=save_path_datafile, 
                    package="SAS") 
    }
    
    if (format == "Stata") {
      library(foreign)
      save_path_datafile <- paste(save_path,".csv",sep="")
      save_path_codefile <- paste(save_path,".do",sep="")
      write.foreign(merged,  
                    codefile=save_path_codefile,
                    datafile=save_path_datafile, 
                    package="Stata") 
    }
  }
  
  if (return.object) return(merged)
    rm(list=ls(all=TRUE))
}


check_variables <- function(file,
                            origin.path) {
  if (file == "personal_register") {
    ## personal register
    per.reg <- read_csv(paste(origin.path,"r_file.csv",sep=""), 
                           header = T, sep = ',')
    return(as.data.frame(names(per.reg)))
  }
  if (file == "personal_data") {
    ## personal data
    per.data <- read_csv(paste(origin.path,"p_file.csv",sep=""), 
                           header = T, sep = ',')
    return(as.data.frame(names(per.data)))
  }
  if (file == "household_register") {
    ## personal register
    hh.reg <- read_csv(paste(origin.path,"d_file.csv",sep=""), 
                           header = T, sep = ',')
    return(as.data.frame(names(hh.reg)))
  }
  if (file == "household_data") {
    ## personal register
    hh.data <- read_csv(paste(origin.path,"h_file.csv",sep=""), 
                           header = T, sep = ',')
    return(as.data.frame(names(hh.data)))
  }
  
#     na.table <- function(x) {
#       x <- factor(x)
#       tbl <- as.data.frame(table(x, useNA = "ifany"))
#       nas <- as.numeric(tbl[is.na(tbl$x),][2])
#       non_nas <- as.numeric(sum(tbl[!is.na(tbl$x),][2]),na.rm=T)
#       na_share <- nas / (nas + non_nas) * 100
#       non_na_share <- non_nas / (nas + non_nas) * 100
#       sum.vector <- c(nas,non_nas,na_share,non_na_share)
#       sum.vector <- round(sum.vector,1)
#       return(sum.vector)
#       }
#     
#     f <- as.data.frame(t(sapply(eusilc.dat,function(x) rbind(na.table(x)))))
#     f$varname <- rownames(f)
#     names(f) <- c("NA's","non NA's","NA share","non NA share","varnames")
#     f <- f[,c(5,1,2,3,4)]
#     f <- f[-1,]
#     return(f)
}

