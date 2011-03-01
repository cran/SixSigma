\name{SixSigma-package}
\alias{SixSigma-package}
\alias{SixSigma}
\docType{package}
\title{Six Sigma Quality }
\description{This package contains functions and utilities to perform
  Statistical Analysis in the Six Sigma manner. Thorugh its outline
  DMAIC (Define, Measure, Analyze, Improve), you can manage several
  Quality Management studies: Gage R&R, Capability Analysis, Control
  Charts, Reliability, OC Curves, Regression and Design of
  Experiments. See ?SixSigma for an Introduction.\cr
}
\details{
  \tabular{ll}{
    Package: \tab SixSigma\cr
    Type: \tab Package\cr
    Version: \tab 0.2\cr
    Date: \tab 2011-01-03\cr
    URL: \tab \url{http://www.proyectum.es/p/six-sigma-with-r.html}\cr
    License: \tab GPL>=2\cr
    Depends: \tab R(>=2.11), lattice, grid\cr
    LazyLoad: \tab yes\cr
    LazyData: \tab yes\cr
  }
  Use the package to perform Six Sigma Methologogy tasks, along its
  breakthrough strategy: Define, Measure, Analyse, Improve, Control (DMAIC)\cr
  Define: Process Map (ss.pMap), Cause and effect Diagram
  (ss.ceDiag);\cr
  Measure: Gage R&R study (ss.rr)\cr
  Soon: other steps
}
\author{
  Emilio Lopez Cano;
  Andres Redchuk;
  Javier M. Moguerza\cr

Maintainer: Emilio Lopez Cano \email{elopez@proyectum.es}
}
\note{The current version includes Gage R&R Study, confidence intervals,
  Process Map and Cause-and-Effect diagram. We plan to deploy several
  versions along the upcoming weeks, with new functions and improving
  those previously deployed. The version 0.2 deals with step 1 and 2 in Six
  Sigma methodology outline(DMAIC): Define and Measure.\cr
  Next versions will refer to the steps:
      3. Analyse\cr
      4. Improve\cr
      5. Control\cr

}
\references{
  Rey Juan Carlos University, Master in Decision Systems Engineering\cr
  <\url{http://www.masteringenieriasistemasdecision.com}/>\cr
  Allen, T. T. (2010). Introduction to Engineering Statistics and Lean
  Six Sigma - Statistical Quality Control and Design of Experiments and
  Systems (Second Edition ed.). London: Springer.\cr
  Chambers, J. M. (2008). Software for data analysis. Programming with
  R. New York: Springer.\cr
  Montgomery, DC (2005). Introduction to Statistical Quality Control
  (Fifth Edition). New York: Wiley&Sons\cr
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