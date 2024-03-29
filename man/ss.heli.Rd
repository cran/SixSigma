% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss.heli.R
\name{ss.heli}
\alias{ss.heli}
\title{Creates a pdf file with the design of the Paper Helicopter}
\usage{
ss.heli()
}
\value{
No value is returned. A pdf file is saved in the working directory
}
\description{
The pdf file contains a template with lines and indications to build the 
paper helicopter described in many SixSigma publications.
}
\details{
The pdf file must be printed in A4 paper, without adjusting size to paper.
}
\note{
See the \code{vignette("HelicopterInstructions")} to see assembling instructions.
}
\examples{
\dontrun{
## ss.heli()
vignette("HelicopterInstructions")
}
}
\references{
George Box.
Teaching engineers experimental design with a paper helicopter.
\emph{Quality Engineering}, 4(3):453--459, 1992.

Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{https://link.springer.com/book/10.1007/978-1-4614-3652-2}.
}
\author{
EL Cano
}
