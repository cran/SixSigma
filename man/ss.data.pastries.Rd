\name{ss.data.pastries}
\alias{ss.data.pastries}
\docType{data}
\title{
Pastries
}
\description{
A data frame with 18 observations of the amount of the CTQ compound in some
pastries from a bakery. There are two runs for each combination of two factors
(laboratory and batch).
}
\usage{data(ss.data.pastries)}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{\code{lab}}{laboratory: a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{batch}}{batch: a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{run}}{identifies the run: a factor with levels \code{1} \code{2}}
    \item{\code{comp}}{proportion of the compound in the pastry: a numeric vector}
  }
}

\source{
Data from the authors of the package.
}
\examples{
data(ss.data.pastries)
}
\keyword{datasets}
