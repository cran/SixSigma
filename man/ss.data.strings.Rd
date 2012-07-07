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

\references{
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
}

\examples{
data(ss.data.strings)
}
\keyword{datasets}
