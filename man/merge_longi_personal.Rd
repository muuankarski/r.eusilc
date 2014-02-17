\name{merge_longi_personal}
\alias{merge_longi_personal}
\title{Merge eu-silc personal level longitudinal raw .csv datasets, personal_register and personal_data}
\usage{
merge_longi_personal(origin.path, destination.path, format)
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
Merge eu-silc personal level longitudinal raw .csv
datasets, personal_register and personal_data
}
\examples{
# per_longi <-  merge_longi_personal(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged", format="RData")
}
\author{
Markus Kainu <markuskainu(at)gmail.com>
}

