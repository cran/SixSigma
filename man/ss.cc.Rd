\name{ss.cc.getd2}
\alias{ss.cc.getd2}
\alias{ss.cc.getd3}
\alias{ss.cc.getc4}
\title{
Functions to find out constants of the relative range distribution.
}
\description{
These functions compute the constants d2, d3 and c4 to get estimators of the
standard deviation to set control limits.
}
\usage{
ss.cc.getd2(n = NA)
ss.cc.getd3(n = NA)
ss.cc.getc4(n = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Sample size
}
}

\value{
A numeric value for the constant.
}

\references{
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
}

\author{
Emilio L. Cano
}

\examples{
ss.cc.getd2(20)
ss.cc.getd3(20)
ss.cc.getc4(20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
