\name{ss.rr}
\alias{ss.rr}

\title{
Gage R & R
}
\description{
Performs Gage R&R analysis. Related to the Measure phase of DMAIC
strategy of Six Sigma.
}
\usage{
ss.rr(var, part, appr, data = "stop('Data' is requiered for lattice graphics)",
main = "Six Sigma Gage R&R Measure", sub = "")
}

\arguments{
  \item{var}{
Measured variate
}
  \item{part}{
Factor for parts
}
  \item{appr}{
Factor for appraisals (operators, machines, ...)
}
  \item{data}{
Data frame containing the variates
}
  \item{main}{
Main title for the graphic output
}
  \item{sub}{
Subtitle for the graphic output (recommended the name of the project)
}
}
\details{
Performs an R&R study for the measured variable, taking in account
  part and appraisal factors. It outputs the sources of Variability, and
  six graphs: bar chart with the sources of Variablity, plots by
  appraisal, part and interaction and x-mean and R control charts.
}
\value{
Analysis of Variance Table. Variance composition and \%Study Var. Graphics.
}
\references{
Montgomery, D. C. (2005). Introduction to Statistical Quality Control
(Fifth Edition ed.). New York: Wiley & Sons, Inc.\cr
Allen, T. T. (2010). Introduction to Engineering Statistics and Lean Six Sigma - Statistical Quality Control and Design of Experiments and Systems (Second Edition ed.). London: Springer.
}
\author{
Emilio Lopez Cano
}

\seealso{
\code{\link{ss.data.rr}}
}
\examples{
data(ss.data.rr)
ss.rr(time1, prototype, operator, data=ss.data.rr, sub="Six Sigma Paper
Helicopter Project")
 }
\keyword{Gage}
\keyword{R&R}
\keyword{Reproducibility}
\keyword{Repeatability}
\keyword{Measure}

