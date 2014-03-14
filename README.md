r.eusilc - R-package for merging eu-silc cross-sectional and longitudinal raw .csv datasets
==================================

This package provides functions for merging raw .csv-files into single household/personal level datafile.

Installation
---------------------------------

```r
library(devtools)
install_github("r.eusilc","muuankarski")
```

Get the documentation by running

```r
library(r.eusilc)
?merge_eusilc
```


Examples
---------------------------------

**Examples out of date!!** See preliminary tutorial at [muuankarski.github.io/r.eusilc/vignettes/r.eusilc_tutorial.html](http://muuankarski.github.io/r.eusilc/vignettes/r.eusilc_tutorial.html)

Try out the ones below instead

```r
library(r.eusilc)

#subsetting
var.per.reg <- c("RB010","RB020")
var.per.data <- c("PB030","PB150")
var.hh.reg <- c("DB010","DB020","DB060")
var.hh.data <- c("HB010","HB020","HB030")
clist <- c("FI","SE","EE")

path.cross.2010 <- "~/demo_data/eusilc_raw/2010/cross_rev3"
path.longi.2010 <- "~/demo_data/eusilc_raw/2010/longi_rev2"
output.2010 <- "~/demo_data/eusilc_merged/2010"

# different scenarious
hh_cross_2010 <- merge_eusilc(origin.path=path.cross.2010,
                              output.path=output.2010,
                              level="household",
                              type="cross-sectional",
                              year="2010",
                              format="csv",
                              subset.vars.per.reg=var.per.reg,
                              subset.vars.per.data=var.per.data,
                              subset.vars.hh.reg=var.hh.reg,
                              subset.vars.hh.data=var.hh.data,
                              subset.countries=clist) 

per_cross_2010 <- merge_eusilc(origin.path=path.cross.2010,
                               output.path=output.2010,
                               level="personal",
                               type="cross-sectional",
                               year="2010",
                               format="csv",
                               subset.vars.per.reg=var.per.reg,
                               subset.vars.per.data=var.per.data,
                               subset.vars.hh.reg=var.hh.reg,
                               subset.vars.hh.data=var.hh.data,
                               subset.countries=clist) 


both_cross_2010 <- merge_eusilc(origin.path=path.cross.2010,
                                output.path=output.2010,
                                level="both",
                                type="cross-sectional",
                                year="2010",
                                format="csv",
                                subset.vars.per.reg=var.per.reg,
                                subset.vars.per.data=var.per.data,
                                subset.vars.hh.reg=var.hh.reg,
                                subset.vars.hh.data=var.hh.data,
                                subset.countries=clist) 


hh_longi_2010 <- merge_eusilc(origin.path=path.longi.2010,
                                output.path=output.2010,
                                level="household",
                                type="longitudinal",
                                year="2010",
                                format="csv",
                                subset.vars.per.reg=var.per.reg,
                                subset.vars.per.data=var.per.data,
                                subset.vars.hh.reg=var.hh.reg,
                                subset.vars.hh.data=var.hh.data,
                                subset.countries=clist) 


per_longi_2010 <- merge_eusilc(origin.path=path.longi.2010,
                               output.path=output.2010,
                              level="personal",
                              type="longitudinal",
                              year="2010",
                              format="csv",
                              subset.vars.per.reg=var.per.reg,
                              subset.vars.per.data=var.per.data,
                              subset.vars.hh.reg=var.hh.reg,
                              subset.vars.hh.data=var.hh.data,
                              subset.countries=clist) 


both_longi_2010 <- merge_eusilc(origin.path=path.longi.2010,
                                output.path=output.2010,
                               level="both",
                               type="longitudinal",
                               year="2010",
                               format="csv",
                               subset.vars.per.reg=var.per.reg,
                               subset.vars.per.data=var.per.data,
                               subset.vars.hh.reg=var.hh.reg,
                               subset.vars.hh.data=var.hh.data,
                               subset.countries=clist) 

```