\name{merge_cross_household}
\alias{merge_cross_household}
\title{Merge eu-silc household level longitudinal raw .csv datasets, household_register and household_data}
\usage{
merge_cross_household(origin.path, destination.path, format,
  data.table = FALSE, subset.vars = "all", subset.countries = "all")
}
\arguments{
  \item{origin.path}{A string. Specify the path where to
  load the original csv files.}

  \item{destination.path}{A string. Specify the path where
  to save the merged file. value=/code{"not_save"} does not
  save the merged file}

  \item{format}{A string. Specify the output format for
  merged data.
  values=/code{"csv","RData","spss","sas","stata"}}

  \item{subset.vars}{A string. Specify subset of variables
  from both datas. /code{"all"} includes all the
  variables.}

  \item{subset.countries}{A string. Specify subset of
  countries. In /code{c("FI","SE")} format.  /code{"all"}
  includes all the countries.}
}
\value{
data.frame
}
\description{
Merge eu-silc household level longitudinal raw .csv
datasets, household_register and household_data
}
\examples{
# hh_cross <-  merge_cross_household(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
}
\author{
Markus Kainu <markuskainu(at)gmail.com>
}

