\name{ss.rr}
\alias{ss.rr}

\title{
Gage R & R (Measure System Assessment)
}
\description{
Performs Gage R&R analysis for the assessment of the measure 
system of a process. Related to the Measure phase of DMAIC
strategy of Six Sigma.
}
\usage{
ss.rr(var, part, appr, 
  data = "stop('Data' is required for lattice graphics)", 
  main = "Six Sigma Gage R&R Study", sub = "")
}

\arguments{
  \item{var}{
Measured variate
}
  \item{part}{
Factor for parts
}
  \item{appr}{
Factor for appraisers (operators, machines, ...)
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
  part and appraiser factors. It outputs the sources of Variability, and
  six graphs: bar chart with the sources of Variability, plots by
  appraiser, part and interaction and x-mean and R control charts.
}
\value{
Analysis of Variance Table. Variance composition and \%Study Var. Graphics.
\item{anovaTable}{The ANOVA table of the model}
\item{varComp}{A matrix with the contribution of each component to the 
	total variation}
\item{studyVar}{A matrix with the contribution to the study variation}
\item{ncat}{Number of distinct categories}
}
\references{
Montgomery, D. C. (2005). Introduction to Statistical Quality Control
(Fifth Edition ed.). New York: Wiley & Sons, Inc.\cr

Allen, T. T. (2010). Introduction to Engineering Statistics and Lean Six Sigma - Statistical Quality Control and Design of Experiments and Systems (Second Edition ed.). London: Springer.
}
\author{
Emilio L. Cano
}

\seealso{
\code{\link{ss.data.rr}}
}
\examples{
data(ss.data.rr)
ss.rr(time1, prototype, operator, data=ss.data.rr, 
	sub="Six Sigma Paper Helicopter Project")
}
\keyword{Gage R&R}
\keyword{Reproducibility}
\keyword{Repeatability}
\keyword{Measure Phase}

