\name{ss.ci}
\alias{ss.ci}
\title{
Confidence Interval for the mean
}
\description{
Computes a confidence interval for the mean of the variable (parameter
or feature of the process), and prints the data, a histogram and the
result of the Shapiro-Wilks normality test.
}
\usage{
ss.ci(x, sigma = "", alpha = 0.05, data = "", xname = "x", 
  approx.z=TRUE, main = "Confidence Interval for the mean",
  sub = "", ss.col = c("#666666", "#BBBBBB", "#CCCCCC", 
                       "#DDDDDD", "#EEEEEE"))
}

\arguments{
  \item{x}{
A numeric vector with the variable data
}
  \item{sigma}{
The population variance, if known
}
  \item{alpha}{
The eqn{\\alpha} error used to compute the \eqn{100*(1-\\alpha)\%} confidence interval
}
  \item{data}{
The data frame containing the vector
}
  \item{xname}{
The name of the variable to show in the graph
}
\item{approx.z}{
If TRUE it uses z statistic instead of t when sigma is unknown and sample size 
is greater than 30
}
  \item{main}{
The main title for the graph
}
  \item{sub}{
The subtitle for the graph (recommended: six sigma project name)
}
  \item{ss.col}{
A vector with colors
}
}
\details{
When the population variance is known, or the size is greater than 30,
it uses z statistic. Otherwise, it is uses t statistic.\cr
If the sample size is lower than 30, a warning is displayed so as to
verify normality.
}
\value{
  The confidence Interval.\cr
  A graph with the figures, the Shapiro-Wilks test,  and a histogram.
}

\author{
Emilio Lopez Cano
}

\seealso{
  \code{\link{ss.data.rr}}
}
\examples{
data(ss.data.rr)
ss.ci(time1, data=ss.data.rr, alpha=0.05,
sub="Paper Helicopter Project | First Flight Time Evaluation", 
  xname="Time")
}
\keyword{Confidence Interval }
\keyword{mean}
\keyword{Normality Test}
