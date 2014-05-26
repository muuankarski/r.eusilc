# var.per.reg <- c("RB010","RB020")
# var.per.data <- c("PB030","PB150")
# var.hh.reg <- c("DB010","DB020","DB060")
# var.hh.data <- c("HB010","HB020","HB030")
# clist <- c("FI","SE","EE")

path.cross.2010 <- "~/data/demo_data/eusilc_subset/2010/cross_rev3/"
path.longi.2010 <- "~/data/demo_data/eusilc_subset/2010/longi_rev2/"
output.2010 <- "~/data/demo_data/eusilc_subset/2010/"


origin.path <- "~/data/demo_data/eusilc_subset/2010/cross_rev3/"
output.path <- "~/data/demo_data/eusilc_subset/2010/"
level <- "household"
type <- "cross-sectional"
year <- 2010
format <- "csv"
subset.vars.per.reg <- "all"
subset.vars.per.data <- "all"
subset.vars.hh.reg <- "all"
subset.vars.hh.data <- "all"
subset.countries <- "all"

# Tee ko hakemisto
hh_cross_2010 <- merge_eusilc(origin.path=path.cross.2010,
                              output.path=output.2010,
                              level="household",
                              type="cross-sectional",
                              year="2010",
                              format="RData",
                              subset.vars.hh.reg = c("DB090"),
                              subset.vars.hh.data = c("HY020","HY022"),
                              subset.countries = c("DE","PL")) 


per_cross_2010 <- merge_eusilc(origin.path=path.cross.2010,
                              output.path=output.2010,
                              level="personal",
                              type="cross-sectional",
                              year="2010",
                              format="RData",
                              subset.vars.per.reg = c("RB050","RB090"),
                              subset.vars.per.data = c("PE010","PL025"),
                              subset.countries = "DE") 

both_cross_2010 <- merge_eusilc(origin.path=path.cross.2010,
                               output.path=output.2010,
                               level="both",
                               type="cross-sectional",
                               year="2010",
                               format="RData",
                               subset.vars.hh.reg = c("DB090"),
                               subset.vars.hh.data = c("HY020","HY022"),
                               subset.vars.per.reg = c("RB050","RB090"),
                               subset.vars.per.data = c("PE010","PL025"),
                               subset.countries = "DE")



hh_longi_2010 <- merge_eusilc(origin.path=path.longi.2010,
                              output.path=output.2010,
                              level="household",
                              type="longitudinal",
                              year="2010",
                              format="RData",
                              subset.vars.hh.reg = c("DB090"),
                              subset.vars.hh.data = c("HY020","HY022"),
                              subset.countries = c("DE","PL")) 


per_longi_2010 <- merge_eusilc(origin.path=path.longi.2010,
                              output.path=output.2010,
                              level="personal",
                              type="longitudinal",
                              year="2010",
                              format="RData",
                              subset.vars.per.reg = c("RB050","RB090"),
                              subset.vars.per.data = c("PE010","PL025"),
                              subset.countries = "DE")

both_longi_2010 <- merge_eusilc(origin.path=path.longi.2010,
                                output.path=output.2010,
                                level="both",
                                type="longitudinal",
                                year="2010",
                                format="RData"),
                                subset.vars.hh.reg = c("DB090"),
                                subset.vars.hh.data = c("HY020","HY022"),
                                subset.vars.per.reg = c("RB050","RB090"),
                                subset.vars.per.data = c("PE010","PL025"),
                                subset.countries = "DE")


d_file <- read.csv("~/data/demo_data/eusilc_subset/2008/longi_rev3/d_file.csv")
h_file <- read.csv("~/data/demo_data/eusilc_subset/2008/longi_rev3/h_file.csv")
p_file <- read.csv("~/data/demo_data/eusilc_subset/2008/longi_rev3/p_file.csv")
r_file <- read.csv("~/data/demo_data/eusilc_subset/2008/longi_rev3/r_file.csv")
