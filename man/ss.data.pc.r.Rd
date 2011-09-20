\name{ss.data.pc.r}
\alias{ss.data.pc.r}
\docType{data}
\title{
Data set for the printer cartridges example, by regions
}
\description{
This data set contains data from a simulated process of printer cartridges filling. 
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
\examples{
data(ss.data.pc.r)
}
\keyword{datasets}
