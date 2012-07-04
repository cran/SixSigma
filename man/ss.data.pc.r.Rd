\name{ss.data.pc.r}
\alias{ss.data.pc.r}
\docType{data}
\title{
Data set for the printer cartridge example, by region
}
\description{
This data set contains data from a simulated process of printer cartridge filling. 
The dataframe contains defects by region of each type of cartridge.
}
\usage{data(ss.data.pc.r)}
\format{
  A data frame with 5 observations on the following 4 variables.
  \describe{
    \item{\code{pc.regions}}{a factor with levels \code{region.1} \code{region.2} \code{region.3} \code{region.4} \code{region.5}}
    \item{\code{pc.def.a}}{a numeric vector for defects type A}
    \item{\code{pc.def.b}}{a numeric vector for defects type B}
    \item{\code{pc.def}}{a numeric vector for total defects}
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
data(ss.data.pc.r)
}
\keyword{datasets}
