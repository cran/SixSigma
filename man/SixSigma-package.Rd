\name{SixSigma-package}
\alias{SixSigma-package}
\alias{SixSigma}
\docType{package}
\title{Six Sigma Tools for Quality and Process Improvement}
\description{This package contains functions and utilities to perform Statistical Analyses in the Six Sigma way. 
 Through the DMAIC cycle (Define, Measure, Analyze, Improve, Control), you can manage several Quality Management 
 studies: Gage R&R, Capability Analysis, Control Charts, Loss Function Analysis, etc. Data frames used in 
 "Six Sigma with R" (Springer, 2012) are also included in the package.
}
\details{
  \tabular{ll}{
    Package: \tab SixSigma\cr
    Type: \tab Package\cr
    Version: \tab 0.6.3\cr
    Date: \tab 2012-09-05\cr
    URL: \tab \url{http://rwiki.sciviews.org/doku.php?id=packages:cran:sixsigma}\cr
    License: \tab GPL>=2\cr
    Depends: \tab R(>=2.12), lattice, grid, ggplot2, nortest, reshape2, plyr, testthat\cr
    LazyLoad: \tab yes\cr
    LazyData: \tab yes\cr
  }
  Use the package to perform Six Sigma Methodology tasks, throughout its
  breakthrough strategy: Define, Measure, Analyze, Improve, Control (DMAIC)\cr
  Define: Process Map (ss.pMap), Cause and effect Diagram
  (ss.ceDiag);\cr
  Measure: Gage R&R study (ss.rr); Capability Analysis (ss.study.ca); 
  Loss Function Analysis (ss.lfa)\cr
  Analyze: Confidence Intervals (ss.ci)\cr
  Soon: further functions
}
\author{
  Emilio L. Cano;
  Andres Redchuk;
  Javier M. Moguerza\cr
Maintainer: Emilio L. Cano \email{emilio.lopez@urjc.es}
}
\note{The current version includes Loss Function Analysis, Gage R&R Study, 
confidence intervals,
  Process Map and Cause-and-Effect diagram. We plan to regularly upload
  updated versions, with new functions and improving
  those previously deployed. The subsequent versions will cover 
  tools for the whole cycle:
  \tabular{rl}{
  	1.\tab Define\cr
  	2.\tab Measure\cr
    3.\tab Analyse\cr
    4.\tab Improve\cr
    5.\tab Control\cr
    }

}
\references{
Allen, T. T. (2010) \emph{Introduction to Engineering Statistics and Lean
  Six Sigma - Statistical Quality Control and Design of Experiments and
  Systems} (Second Edition ed.). London: Springer.\cr
  
Box, G. (1991). Teaching engineers experimental design with 
	a paper helicopter. Report 76, Center for Quality and 
	Productivity Improvement. University of Wisconsin.\cr
	
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
  
Chambers, J. M. (2008) \emph{Software for data analysis. Programming with
  R} New York: Springer.\cr
  
Montgomery, DC (2008) \emph{Introduction to Statistical Quality Control}
  (Sixth Edition). New York: Wiley&Sons\cr
  

Wikipedia, \url{http://en.wikipedia.org/wiki/Six_Sigma}
}
\seealso{
  \code{\link{ss.ceDiag}};
  \code{\link{ss.pMap}};
  \code{\link{ss.rr}};
  \code{\link{ss.ci}};
  \code{\link{ss.study.ca}};
}
\examples{
example(ss.ci)
example(ss.study.ca)
example(ss.rr)
example(ss.lf)
example(ss.lfa)
example(ss.ceDiag)
example(ss.pMap)
example(ss.ca.yield)
example(ss.ca.z)
example(ss.ca.cp)
example(ss.ca.cpk)
}

\keyword{sixsigma}
\keyword{quality}
\keyword{package}