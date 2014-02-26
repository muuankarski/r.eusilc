\name{r.eusilc-package}
\alias{r.eusilc-package}
\alias{r.eusilc}
\docType{package}
\title{
Merge eu-silc cross-sectional and longitudinal raw .csv datasets
~~ package title ~~
}
\description{
Merge eu-silc cross-sectional and longitudinal raw .csv datasets into single data.frame that can be used in analysis
}
\details{
\tabular{ll}{
Package: \tab r.eusilc\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-02-12\cr
License: \tab FreeBSD\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Markus Kainu

Maintainer: Markus Kainu <markuskainu@gmail.com>
}
\references{

}
\keyword{ eu-silc }
\seealso{
\code{\link[<pkg>:<pkg>-package]{<pkg>}}
}
\examples{
dat_per_longi <- merge_eusilc(origin.path="~/data/eu_silc/2010/longi_rev2",
                          output.path="~/data_temp/",
                          level="personal",
                          type="longitudinal",
                          year="2010",
                          format="RData",
                          subset.vars="all",
                          subset.countries="all") 
}
