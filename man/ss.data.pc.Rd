\name{ss.data.pc}
\alias{ss.data.pc}
\docType{data}
\title{
Data set for the printer cartridges example
}
\description{
This data set contains data from a simulated process of printer cartridges filling.
}
\usage{data(ss.data.pc)}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{pc.col}}{a factor with levels \code{C} \code{B} for the colour}
    \item{\code{pc.filler}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{pc.volume}}{a numeric vector}
    \item{\code{pc.density}}{a numeric vector}
    \item{\code{pc.batch}}{a numeric vector}
    \item{\code{pc.op}}{a factor with levels \code{A} \code{B} \code{C} \code{D} 
    for the operator}
  }
}

\source{
The data have been obtained by the authors by simulation.
}

\examples{
data(ss.data.pc)
}
\keyword{datasets}
