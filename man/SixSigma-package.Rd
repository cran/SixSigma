\name{SixSigma-package}
\alias{SixSigma-package}
\alias{SixSigma}
\docType{package}
\title{Six Sigma Tools for Quality and Process Improvement}
\description{This package contains functions and utilities to 
perform Statistical Analysis in the Six Sigma way. 
Through the DMAIC cycle (Define, Measure, Analyze, Improve, Control), 
you can manage several Quality Management studies: Gage R&R, Capability 
Analysis, Control Charts, Reliability, Regression 
and Design of Experiments. 
}
\details{
  \tabular{ll}{
    Package: \tab SixSigma\cr
    Type: \tab Package\cr
    Version: \tab 0.5.0\cr
    Date: \tab 2012-01-04\cr
    URL: \tab \url{http://rwiki.sciviews.org/doku.php?id=packages:cran:sixsigma}\cr
    License: \tab GPL>=2\cr
    Depends: \tab R(>=2.11), lattice, grid\cr
    LazyLoad: \tab yes\cr
    LazyData: \tab yes\cr
  }
  Use the package to perform Six Sigma Methodology tasks, throughout its
  breakthrough strategy: Define, Measure, Analyse, Improve, Control (DMAIC)\cr
  Define: Process Map (ss.pMap), Cause and effect Diagram
  (ss.ceDiag);\cr
  Measure: Gage R&R study (ss.rr); Capability Analysis (ss.study.ca); 
  Loss Function Analysis (ss.lfa)\cr
  Analyze: Confidence Intervals (ss.ci)
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
  Systems} (Second Edition ed.). London: Springer.

Box, G. (1991). Teaching engineers experimental design with 
	a paper helicopter. Report 76, Center for Quality and 
	Productivity Improvement. University of Wisconsin.\cr
	
Chambers, J. M. (2008) \emph{Software for data analysis. Programming with
  R} New York: Springer.

Montgomery, DC (2005) \emph{Introduction to Statistical Quality Control}
  (Fifth Edition). New York: Wiley&Sons

Rey Juan Carlos University, Master in Decision Systems Engineering\cr
  <\url{http://www.masteringenieriasistemasdecision.com}/>
  
  \url{http://en.wikipedia.org/wiki/Six_Sigma}
}
\seealso{
  \code{\link{ss.ceDiag}};
  \code{\link{ss.pMap}};
  \code{\link{ss.rr}};
  \code{\link{ss.ci}}
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