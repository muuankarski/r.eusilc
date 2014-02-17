\name{merge_longi_household}
\alias{merge_longi_household}
\title{Merge eu-silc household level longitudinal raw .csv datasets, household_register and household_data}
\usage{
merge_longi_household(origin.path, destination.path, format)
}
\arguments{
  \item{origin.path}{A string. Specify the path where to
  load the original csv files.}

  \item{destination.path}{A string. Specify the path where
  to save the merged file. value=/code{"not_save"} does not
  save the merged file}
}
\value{
data.frame
}
\description{
Merge eu-silc household level longitudinal raw .csv
datasets, household_register and household_data
}
\examples{
# hh_longi <-  merge_longi_household(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
}
\author{
Markus Kainu <markuskainu(at)gmail.com>
}

