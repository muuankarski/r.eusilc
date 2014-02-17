r.eusilc - R-package for merging eu-silc cross-sectional and longitudinal raw .csv datasets
==================================

This package provides functions for merging raw .csv-files into single household/personal level datafile. Also, package provides functions for pooling the data from different waves into single data file. Procedures will be first implemented for longitudinal files.

Installation
---------------------------------

```r
library(devtools)
install_github("r.eusilc","muuankarski")


```

Preparations
----------------------------------

1. make sure you have enough RAM memory in your computer. For smooth workflow using all waves & countries in longitudinal data I recommend at least 16GB
2. Install R and libraries `data.table` and `devtools`
3. ...

Only modification have to be done prior running this code is to **change original .csv-filenames into p_file, r_file, h_file and d_file**. I makes the code easier to maintain. Difference between wave, data type and revision version is within folder structure.

