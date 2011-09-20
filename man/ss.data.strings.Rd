\name{ss.data.strings}
\alias{ss.data.strings}
\docType{data}
\title{
Data set for the Guitar Strings example
}
\description{
This data set contains data from a simulated process of guitar strings production.
}
\usage{data(ss.data.strings)}
\format{
  A data frame with 120 observations on the following 6 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{type}}{a factor with levels \code{A5} \code{B2} \code{D4} \code{E1} \code{E6} \code{G3}}
    \item{\code{res}}{a numeric vector for resistance}
    \item{\code{len}}{a numeric vector for length}
    \item{\code{sound}}{a numeric vector for}
    \item{\code{power}}{a numeric vector}
  }
}

\source{
The data have been obtained by the authors by simulation.
}

\examples{
data(ss.data.strings)
}
\keyword{datasets}
