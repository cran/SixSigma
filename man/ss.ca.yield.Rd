\name{ss.ca.yield}
\alias{ss.ca.yield}
\title{
Main calculations regarding The Voice of the Process in SixSigma: Yield, FTY, RTY,
DPMO
}
\description{
Computes the Yield, First Time Yield, Rolled Throughput Yield and Defects
per Million Opportunities of a process.
}
\usage{
ss.ca.yield(defects = 0, rework = 0, opportunities = 1)
}
\arguments{
  \item{defects}{
A vector with the number of defects in each product/batch, ...
}
  \item{rework}{
A vector with the number of items/parts reworked
}
  \item{opportunities}{
A numeric value with the size or length of the product/batch
}
}
\details{
The three arguments must have the same length.
}
\value{
  \item{Yield }{Number of good stuff / Total items}
  \item{FTY }{(Total - scrap - rework) / Total }
  \item{RTY }{prod(FTY)}
  \item{DPMO}{Defects per Million Opportunities}
}

\references{
Gygi C, DeCarlo N, Williams B (2005) \emph{Six sigma for dummies}. --For dummies,
  Wiley Pub.\cr
  	
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
}

\author{
Emilio L. Cano
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{help}}, ~~~
%}
\examples{
ss.ca.yield(c(3,5,12),c(1,2,4),1915)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Yield}
\keyword{ FTY }
\keyword{ RTY }
\keyword{ DPMO }
