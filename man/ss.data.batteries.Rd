\name{ss.data.batteries}
\alias{ss.data.batteries}
\docType{data}
\title{
Data for the batteries example
}
\description{
This is a simulated data set of 18 measurements of the voltage of batteries using different
voltmeters.
}
\usage{data(ss.data.batteries)}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{\code{voltmeter}}{a factor with levels \code{1} \code{2}}
    \item{\code{battery}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{run}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{voltage}}{a numeric vector}
  }
}

\source{
Simulated by the authors
}
\references{
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
}
\examples{
data(ss.data.batteries)
}
\keyword{datasets}
