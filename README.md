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

See preliminary tutorial at [muuankarski.github.io/r.eusilc/vignettes/r.eusilc_tutorial.html](http://muuankarski.github.io/r.eusilc/vignettes/r.eusilc_tutorial.html)


### Install

```{r, eval=FALSE}
library(devtools)
install_github("r.eusilc","muuankarski")
library(r.eusilc)
```

### Merge individual level dataset with household level variables from longitudinal data

```{rcase4data, eval=FALSE}
library(r.eusilc)
both_longi_2010 <- merge_eusilc(path.personal.register  = "~/demo_data/eusilc_raw/2010/longi_rev3/UDB_l10R_ver 2010-4 from 01-03-2014.csv",
                               path.personal.data      = "~/demo_data/eusilc_raw/2010/longi_rev3/UDB_l10P_ver 2010-4 from 01-03-2014.csv",
                               path.household.register = "~/demo_data/eusilc_raw/2010/longi_rev3/UDB_l10D_ver 2010-4 from 01-03-2014.csv",
                               path.household.data     = "~/demo_data/eusilc_raw/2010/longi_rev3/UDB_l10H_ver 2010-4 from 01-03-2014.csv",
                               output.path="~/demo_data/eusilc_merged/2010",
                               level="both",
                               type="longitudinal",
                               year="2010",
                               format="RData",
                               subset.vars.per.reg="all",
                               subset.vars.per.data="all",
                               subset.vars.hh.reg="all",
                               subset.vars.hh.data="all",
                               subset.countries="all") 
```