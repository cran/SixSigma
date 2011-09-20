\name{ss.lf}
\alias{ss.lf}
\title{
Evaluates the Loss Function for a process.
}
\description{
The quality loss function is one of the tools of the Six Sigma methodology.
The function assigns a cost to an observed value, that is larger as far as it
is from the target. 
}
\usage{
ss.lf(lfa.Y1, lfa.Delta, lfa.Y0, lfa.L0)
}

\arguments{
  \item{lfa.Y1}{
The observed value of the CTQ (critical to quality) characteristic
that will be evaluated.
}
  \item{lfa.Delta}{
The tolerance for the CTQ.
}
  \item{lfa.Y0}{
The target for the CTQ.
}
  \item{lfa.L0}{
The cost of poor quality when the characteristic is \eqn{Y_0 + \Delta}.
}
}

\value{
\item{ss.lf}{A number with the evaluated function at \eqn{Y_1}}
}

\references{
Taguchi G, Chowdhury S,Wu Y (2005) \emph{Taguchi's quality engineering handbook}. John
Wiley
}
\author{
Emilio Lopez-Cano
}

\seealso{
\code{\link{ss.lfa}}
}
\examples{

#Example bolts: evaluate LF at 10.5 if Target=10, Tolerance=0.5, L_0=0.001
ss.lf(10.5, 0.5, 10, 0.001)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Loss Function}
\keyword{Taguchi}
\keyword{Define Phase}
