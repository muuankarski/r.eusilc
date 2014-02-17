










load("~/workspace/data/eu_silc/2008/longi_rev3/per_merge.RData")
per_merge08 <- per_merge
load("~/workspace/data/eu_silc/2009/longi_rev3/per_merge.RData")
per_merge09 <- per_merge
load("~/workspace/data/eu_silc/2010/longi_rev2/per_merge.RData")
per_merge10 <- per_merge
load("~/workspace/data/eu_silc/2011/longi_rev0/per_merge.RData")
per_merge11 <- per_merge
rm(per_merge)

# Case Greece
## Country code has changed from 2008 to 2009 from GR to EL
## Let's recode the 2008 to match 2009/10
per_merge08$RB020 <- as.character(per_merge08$RB020)
per_merge08$RB020[per_merge08$RB020 == "GR"] <- "EL"
per_merge08$RB020 <- factor(per_merge08$RB020)

# A joint variable of var (PL030/PL031",  # Self-defined current economic status)
# have to constructed before subsetting the data
#
# let's code var econStatus by taking the PL031 as a bechmark
# before making any corrections let's recode var PL030 using 
# PL031 classes into var PL030rec
# 2008
per_merge08$PL030rec[per_merge08$PL030 == 1] <- 1
per_merge08$PL030rec[per_merge08$PL030 == 2] <- 2
per_merge08$PL030rec[per_merge08$PL030 == 3] <- 5
per_merge08$PL030rec[per_merge08$PL030 == 4] <- 6
per_merge08$PL030rec[per_merge08$PL030 == 5] <- 7
per_merge08$PL030rec[per_merge08$PL030 == 6] <- 8
per_merge08$PL030rec[per_merge08$PL030 == 7] <- 9
per_merge08$PL030rec[per_merge08$PL030 == 8] <- 10
per_merge08$PL030rec[per_merge08$PL030 == 9] <- 11
# 2009
per_merge09$PL030rec[per_merge09$PL030 == 1] <- 1
per_merge09$PL030rec[per_merge09$PL030 == 2] <- 2
per_merge09$PL030rec[per_merge09$PL030 == 3] <- 5
per_merge09$PL030rec[per_merge09$PL030 == 4] <- 6
per_merge09$PL030rec[per_merge09$PL030 == 5] <- 7
per_merge09$PL030rec[per_merge09$PL030 == 6] <- 8
per_merge09$PL030rec[per_merge09$PL030 == 7] <- 9
per_merge09$PL030rec[per_merge09$PL030 == 8] <- 10
per_merge09$PL030rec[per_merge09$PL030 == 9] <- 11
# 2010
per_merge10$PL030rec[per_merge10$PL030 == 1] <- 1
per_merge10$PL030rec[per_merge10$PL030 == 2] <- 2
per_merge10$PL030rec[per_merge10$PL030 == 3] <- 5
per_merge10$PL030rec[per_merge10$PL030 == 4] <- 6
per_merge10$PL030rec[per_merge10$PL030 == 5] <- 7
per_merge10$PL030rec[per_merge10$PL030 == 6] <- 8
per_merge10$PL030rec[per_merge10$PL030 == 7] <- 9
per_merge10$PL030rec[per_merge10$PL030 == 8] <- 10
per_merge10$PL030rec[per_merge10$PL030 == 9] <- 11
# 2011
per_merge11$PL030rec[per_merge11$PL030 == 1] <- 1
per_merge11$PL030rec[per_merge11$PL030 == 2] <- 2
per_merge11$PL030rec[per_merge11$PL030 == 3] <- 5
per_merge11$PL030rec[per_merge11$PL030 == 4] <- 6
per_merge11$PL030rec[per_merge11$PL030 == 5] <- 7
per_merge11$PL030rec[per_merge11$PL030 == 6] <- 8
per_merge11$PL030rec[per_merge11$PL030 == 7] <- 9
per_merge11$PL030rec[per_merge11$PL030 == 8] <- 10
per_merge11$PL030rec[per_merge11$PL030 == 9] <- 11

# then we have to fill in missing values in PL031 with
# values in PL030rec
# let's create a PL031 with only NA values
per_merge08$PL031 <- NA

per_merge08$econStatus <- ifelse(is.na(per_merge08$PL031),  per_merge08$PL030rec, per_merge08$PL031)
per_merge09$econStatus <- ifelse(is.na(per_merge09$PL031),  per_merge09$PL030rec, per_merge09$PL031)
per_merge10$econStatus <- ifelse(is.na(per_merge10$PL031),  per_merge10$PL030rec, per_merge10$PL031)
per_merge11$econStatus <- ifelse(is.na(per_merge11$PL031),  per_merge11$PL030rec, per_merge11$PL031)

# list of variables to be analysed
var.list <- c("PER_ID_Y", # unique year, cntry, personal ID
              "PER_ID", # unique cntry, personal ID
              "RB010",  # year
              "RB020",  # cntry
              "RB030",  # Personal ID
              "RB040",  # Current Household ID
              "RB060",  # Personal base weight
              "RB080",  # Year of birth
              "RB090",  # Sex
              "RB110",  # Membership status
              "RB210",  # Basic activity status
              "RB230",  # Mother ID
              "RX010",  # Age at the date of the interview
              "RX020",  # Age at the end of the income reference period
              # ----------- from personal data ----------------------
              "PB190",  # Marital status
              "PB200",  # Consensual union
              "econStatus",  # Self-defined current economic status
              "PY140G", # Education-related allowances(gross)
              "PL040",  # Status in employment
              "PY010N", # Employee cash or near cash income(gross)
              "PY010G", # Employee cash or near cash income(gross)
              "PY140N", # Education-related allowances
              "PY100N", # Old-age benefits(net)
              "PL210A", # Main activity on January
              "PL210B", # Main activity on February
              "PL210C", # Main activity on March
              "PL210D", # Main activity on April
              "PL210E", # Main activity on May
              "PL210F", # Main activity on June
              "PL210G", # Main activity on July
              "PL210H", # Main activity on August
              "PL210I", # Main activity on September
              "PL210J", # Main activity on October
              "PL210K", # Main activity on November
              "PL210L"  # Main activity on December
)

dat <- rbind(per_merge08[,var.list],
             per_merge09[,var.list],
             per_merge10[,var.list],
             per_merge11[,var.list])

# merge the panels datas
dat$dup <- duplicated(dat$PER_ID_Y)
dat.uniq <- dat[dat$dup == FALSE,]
dat.uniq$dup <- NULL
dat.per <- dat.uniq


# Load the merged household level panel datasets
load("~/workspace/data/eu_silc/2008/longi_rev3/hh_merge.RData")
hh_merge08 <- hh_merge
load("~/workspace/data/eu_silc/2009/longi_rev3/hh_merge.RData")
hh_merge09 <- hh_merge
load("~/workspace/data/eu_silc/2010/longi_rev2/hh_merge.RData")
hh_merge10 <- hh_merge
load("~/workspace/data/eu_silc/2011/longi_rev0/hh_merge.RData")
hh_merge11 <- hh_merge
rm(hh_merge)


# list of variables to be analysed
var.list <- c("HH_ID_Y", # unique year, cntry, personal ID
              "HH_ID", # unique cntry, personal ID
              "DB010",  # year
              "DB020",  # cntry
              "DB030",  # household ID
              "DB110",  # Household status
              "HY020",  # Total disposable income
              "HX050",  # Equivivalized household size
              "HX090",  # Equivivalized household income
              "HX100",  # Income quintiles
              "HY050N") # Family/Children related allowances (net)

dat <- rbind(hh_merge08[,var.list],
             hh_merge09[,var.list],
             hh_merge10[,var.list])

dat$dup <- duplicated(dat$HH_ID)
dat.uniq <- dat[dat$dup == FALSE,]
dat.uniq$dup <- NULL
dat.hh <- dat.uniq
dat.hh <- dat.hh[!is.na(dat.hh$HX050), ]
## Merge some files from household data
dat.per <- merge(dat.per,dat.hh, 
                 by.x=c("RB010","RB020","RB040"),
                 by.y=c("DB010","DB020","DB030"),
                 all.x=TRUE)
save(dat.per, file="data/dat.per.RData")