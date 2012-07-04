\name{ss.data.rr}
\alias{ss.data.rr}
\docType{data}
\title{
Gage R&R data
}
\description{
Example data for Measure phase of the Six Sigma methodology.
}
\usage{data(ss.data.rr)}
\format{
  A data frame with 27 observations on the following 5 variables.
  \describe{
       \item{\code{prototype}}{a factor with levels \code{prot #1}
	 \code{prot #2} \code{prot #3}}
    \item{\code{operator}}{a factor with levels \code{op #1} \code{op
	#2} \code{op #3}}
    \item{\code{run}}{a factor with levels \code{run #1} \code{run #2} \code{run #3}}
    \item{\code{time1}}{a numeric vector}
    \item{\code{time2}}{a numeric vector}
  }
}
\details{
  The result of the classical 'Paper Helicopter project' experiment. The flight
  time has been measured 54 times: time1 and time2 are the measure
  before and after the operators training. There are trhee runs for each
  of the trhee operators and three prototypes.
}
\source{
Our own data experience
}
\references{
Allen, T. T. (2010). Introduction to Engineering Statistics and Lean
  Six Sigma - Statistical Quality Control and Design of Experiments and
  Systems (Second Edition ed.). London: Springer.\cr
  
Box, G. (1991). Teaching engineers experimental design with a paper helicopter. Report 76, Center for
  Quality and Productivity Improvement. University of Wisconsin.\cr

Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
}

\examples{
data(ss.data.rr)
str(ss.data.rr)
}
\keyword{datasets}
\keyword{R&R}
\keyword{Measure}
