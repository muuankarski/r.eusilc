\name{merge_longitudinal}
\alias{merge_longitudinal}
\title{Merge eu-silc cross-sectional and longitudinal raw .csv datasets}
\usage{
merge_longitudinal(origin.path, destination.path)
}
\arguments{
  \item{origin.path}{A string. Specify the path where to
  load the original csv files.}

  \item{destination.path}{A string. Specify the path where
  to save the merged file.}
}
\value{
data.frame
}
\description{
Merge eu-silc cross-sectional and longitudinal raw .csv
datasets
}
\examples{
# merge_longitudinal(origin.path = "~/eusilc_raw", destination.path="~/eusilc_merged")
}
\author{
Markus Kainu <markuskainu(at)gmail.com>
}

