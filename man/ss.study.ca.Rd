\name{ss.study.ca}
\alias{ss.study.ca}
\title{
Graphs and figures for a Capability Study
}
\description{
Plots a Histogram with density lines about the data of a process. Check normality
with qqplot and normality tests. Shows the Specification Limits and the 
Capability Indices.
}
\usage{
ss.study.ca(xST, xLT = NA, LSL = NA, USL = NA, Target = NA, 
alpha = 0.05, f.na.rm = TRUE, f.main = "Six Sigma Capability Analysis Study", 
f.sub = "")
}
\arguments{
  \item{xST}{
Short Term process performance data 
}
  \item{xLT}{
Long Term process performance data 
}
  \item{LSL}{
Lower Specification Limit of the process
}
  \item{USL}{
Upper Specification Limit of the process
}
  \item{Target}{
Target of the process
}
  \item{alpha}{
Type I error for the Confidence Interval
}
  \item{f.na.rm}{
If TRUE NA data will be removed
}
  \item{f.main}{
Main Title for the graphic output
}
  \item{f.sub}{
Subtitle for the graphic output
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Figures and plot for Capability Analysis
}
\references{
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr
  
Montgomery, DC (2008) \emph{Introduction to Statistical Quality Control}
  (Sixth Edition). New York: Wiley&Sons
}
\author{
Emilio L. Cano
}

\seealso{
\code{\link{ss.ca.cp}}, ~~~
}
\examples{
	ss.study.ca(ss.data.ca$Volume, rnorm(40, 753, 3), 
		LSL = 740, USL = 760, T = 750, alpha = 0.05, 
 			f.sub = "Winery Project")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Capability Analysis }
\keyword{ SixSigma Studies }
