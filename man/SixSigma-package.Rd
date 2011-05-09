\name{SixSigma-package}
\alias{SixSigma-package}
\alias{SixSigma}
\docType{package}
\title{Six Sigma Tools for Quality and Process Improvement}
\description{This package contains functions and utilities to 
perform Statistical Analysis in the Six Sigma manner. 
Through its outline DMAIC (Define, Measure, Analyze, Improve, Control), 
you can manage several Quality Management studies: Gage R&R, Capability 
Analysis, Control Charts, Reliability, OC Curves, Regression 
and Design of Experiments. See ?SixSigma for an Introduction.
}
\details{
  \tabular{ll}{
    Package: \tab SixSigma\cr
    Type: \tab Package\cr
    Version: \tab 0.2.1\cr
    Date: \tab 2011-03-30\cr
    URL: \tab \url{http://rwiki.sciviews.org/doku.php?id=packages:cran:sixsigma}\cr
    License: \tab GPL>=2\cr
    Depends: \tab R(>=2.11), lattice, grid\cr
    LazyLoad: \tab yes\cr
    LazyData: \tab yes\cr
  }
  Use the package to perform Six Sigma Methodology tasks, along its
  breakthrough strategy: Define, Measure, Analyse, Improve, Control (DMAIC)\cr
  Define: Process Map (ss.pMap), Cause and effect Diagram
  (ss.ceDiag);\cr
  Measure: Gage R&R study (ss.rr)\cr
  Analyze: Confidence Intervals
  Soon: other steps
}
\author{
  Emilio Lopez;
  Andres Redchuk;
  Javier M. Moguerza\cr

Maintainer: Emilio Lopez \email{elopez@proyectum.es}
}
\note{The current version includes Gage R&R Study, confidence intervals,
  Process Map and Cause-and-Effect diagram. We plan to deploy several
  versions along the upcoming weeks, with new functions and improving
  those previously deployed. The version 0.2.1 deals with step 1 and 2 in Six
  Sigma methodology outline(DMAIC): Define and Measure.\cr
  Next versions will refer to the complete cycle:
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
#Process Map
inputs.overall<-c("operators", "tools", "raw material", "facilities")
outputs.overall<-c("helicopter")
procs<-c("INSPECTION", "ASSEMBLY", "TEST", "LABELING")
#Inputs of process "i" are inputs of process "i+1"
input.output<-vector(mode="list",length=length(procs))
input.output[1]<-list(c("sheets", "OTRO"))
input.output[2]<-list(c("sheets"))
input.output[3]<-list(c("helicopter"))
input.output[4]<-list(c("helicopter"))

#Parameters of each process
x.parameters<-vector(mode="list",length=length(procs))
x.parameters[1]<-list(c(list(c("width", "NC")),list(c("operator", "C")),
list(c("Measure pattern", "P")), list(c("discard", "P"))))
x.parameters[2]<-list(c(list(c("operator", "C")),list(c("cut", "P")),
list(c("fix", "P")), list(c("rotor.width", "C")),list(c("rotor.length",
"C")), list(c("paperclip", "C")), list(c("tape", "C"))))
x.parameters[3]<-list(c(list(c("operator", "C")),list(c("throw", "P")),
list(c("discard", "P")), list(c("environment", "N"))))
x.parameters[4]<-list(c(list(c("operator", "C")),list(c("label", "P"))))

#Features of each process
y.features<-vector(mode="list",length=length(procs))
y.features[1]<-list(c(list(c("ok", "Cr"))))
y.features[2]<-list(c(list(c("weight", "Cr"))))
y.features[3]<-list(c(list(c("time", "Cr"))))
y.features[4]<-list(c(list(c("label", "Cr"))))

ss.pMap(procs, inputs.overall, outputs.overall,
     input.output, x.parameters, y.features, main="Six Sigma Process Map",
     sub="Paper Helicopter Manufacturing Project")

#Cause and effect
effect<-"Flight Time"
causes.gr<-c("Operator", "Environment", "Tools", "Design", "Raw.Material",
             "Measure.Tool", "test")
causes<-vector(mode="list", length=length(causes.gr))
causes[1]<-list(c("operator #1", "operator #2", "operator #32"))
causes[2]<-list(c("height", "cleaning"))
causes[3]<-list(c("scisors", "tape"))
causes[4]<-list(c("rotor.length", "rotor.width2", "paperclip"))
causes[5]<-list(c("thickness", "marks"))
causes[6]<-list(c("callibrate", "model"))
causes[7]<-list(c("testcause"))

ss.ceDiag(effect, causes.gr, causes,
main="Six Sigma Cause-and-effect Diagram",
sub="Paper Helicopter Manufacturing Project")

data(ss.data.rr)
ss.rr(time1, prototype, operator, data=ss.data.rr, sub="Six Sigma Paper
Helicopter Project")
ss.ci(time1, data=ss.data.rr, alpha=0.05,
sub="Paper Helicopter Project | First Flight Time Evaluation", xname="Time")
}
\keyword{sixsigma}
\keyword{quality}
\keyword{package}