\name{ss.data.ca}
\alias{ss.data.ca}
\docType{data}
\title{
Data for a filling process in a winery
}
\description{
The only field of the data is the volume measured in 20 bottles.
}
\usage{data(ss.data.ca)}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{Volume}}{a numeric vector (volume in cl}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Package Authors
}

\references{
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
}

\examples{
data(ss.data.ca)
str(ss.data.ca)
}
\keyword{datasets}
\keyword{ Capability Analysis }
