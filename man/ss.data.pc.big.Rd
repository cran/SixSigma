\name{ss.data.pc.big}
\alias{ss.data.pc.big}
\docType{data}
\title{
Big data set for the printer cartridges example
}
\description{
This data set contains data from a simulated process of printer cartridges filling with
complete replications.
}
\usage{data(ss.data.pc.big)}
\format{
  A data frame with 72 observations on the following 5 variables, 
  \describe{
    \item{\code{filler}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{batch}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{col}}{a factor with levels \code{B} \code{C}}
    \item{\code{operator}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{volume}}{a numeric vector}
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
str(ss.data.pc.big)
head(ss.data.pc.big)
}
\keyword{datasets}
