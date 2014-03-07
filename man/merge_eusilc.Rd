\name{merge_eusilc}
\alias{merge_eusilc}
\title{Merge eu-silc personal level cross-sectional raw .csv datasets, personal_register and personal_data}
\usage{
merge_eusilc(origin.path, output.path, level, type, year, format,
  subset.vars = "all", subset.countries = "all")
}
\arguments{
  \item{origin.path}{A string. Specify the path where to
  load the original csv files.}

  \item{destination.path}{A string. Specify the path where
  to save the merged file. value=\code{"not_save"} does not
  save the merged file}

  \item{level}{A string. Specify the whether to merge
  \code{"personal"}, \code{"household"} or \code{"both"}
  level datas. Both stands for merging all the four datas
  at personal level}

  \item{type}{A string. Specify the whether to merge
  \code{"cross-sectional"} or \code{"longitudinal"} level
  datas}

  \item{year}{A string. Specify the year from what year
  data is in question}

  \item{format}{A string. Specify the output format for
  merged data. values=\code{"csv","RData","SPSS","SAS" or
  "Stata"}}

  \item{subset.vars}{A string. Specify subset of variables
  from both datas. \code{"all"} includes all the
  variables.}

  \item{subset.countries}{A string. Specify subset of
  countries. In \code{c("FI","SE")} format.  \code{"all"}
  includes all the countries.}
}
\value{
data.frame
}
\description{
Merge eu-silc personal level cross-sectional raw .csv
datasets, personal_register and personal_data
}
\examples{
# dat_per_longi <- merge_eusilc(origin.path="~/data/eu_silc/2010/longi_rev2", output.path="~/data_temp/",level="personal",type="longitudinal",year="2010",format="RData",subset.vars="all",subset.countries="all")
}
\author{
Markus Kainu <markuskainu(at)gmail.com>
}

