r.eusilc - R-package for merging eu-silc cross-sectional and longitudinal raw .csv datasets
==================================

This package provides functions for merging raw .csv-files into single household/personal level datafile.

Installation
---------------------------------

```r
library(devtools)
install_github("r.eusilc","muuankarski")
```

Examples
---------------------------------


```r
dat_hh_cross <- merge_eusilc(origin.path="~/data/eu_silc/2010/cross_rev3",
                          output.path="~/data_temp/",
                          level="household",
                          type="cross-sectional",
                          year="2010",
                          format="RData",
                          subset.vars="all",
                          subset.countries="all") 


dat_per_longi <- merge_eusilc(origin.path="~/data/eu_silc/2010/longi_rev2",
                          output.path="~/data_temp/",
                          level="personal",
                          type="longitudinal",
                          year="2010",
                          format="RData",
                          subset.vars="all",
                          subset.countries="all") 


dat_both_longi <- merge_eusilc(origin.path="~/data/eu_silc/2010/longi_rev2",
                              output.path="~/data_temp/",
                              level="both",
                              type="longitudinal",
                              year="2010",
                              format="RData",
                              subset.vars="all",
                              subset.countries="all") 

```

Look [muuankarski.github.io/r-eusilc-workshop/](http://muuankarski.github.io/r-eusilc-workshop/) for further examples.

<!--

Preparations
----------------------------------

1. make sure you have enough RAM memory in your computer. For smooth workflow using all waves & countries in longitudinal data I recommend at least 16GB
2. Install R and libraries `data.table` and `devtools`
3. ...

Only modification have to be done prior running this code is to **change original .csv-filenames into p_file, r_file, h_file and d_file**. I makes the code easier to maintain. Difference between wave, data type and revision version is within folder structure.

-->