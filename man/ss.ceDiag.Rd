\name{ss.ceDiag}
\alias{ss.ceDiag}
\title{
Cause and Effect Diagram
}
\description{
Represents a Cause and Effect Diagram by cause group.
}
\usage{
ss.ceDiag(effect, causes.gr, causes,
main = "Six Sigma Cause-and-effect Diagram", sub, ss.col = c("#666666",
"#BBBBBB", "#CCCCCC", "#DDDDDD","#EEEEEE"))
}
\arguments{
  \item{effect}{
A short character string that represents the effect we want to analyse.
}
  \item{causes.gr}{
A vector of characters that represents the causes groups.
}
  \item{causes}{
A vector with lists that represents the individual causes for each
  causes group.
}
  \item{main}{
Main title for the diagram
}
  \item{sub}{
Subtitle for the diagram (recommended the Six Sigma project name)
}
  \item{ss.col}{
A vector of colors for a personalized drawing. At least five colors,
  sorted by descendant intensity
}
}

\value{
A drawing of the causes and effect with "fish-bone" shape
}
\details{
The default value for ss.col is c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD",
  "#EEEEEE"), a grayscale style. You can pass any accepted colour string.
}
\references{
Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andres. 2012.
\emph{Six Sigma with {R}. Statistical Engineering for Process
  Improvement}, Use R!, vol. 36. Springer, New York.
  \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.\cr

Wikipedia, \url{http://en.wikipedia.org/wiki/Ishikawa_diagram}
}
\author{
Emilio L. Cano
}
\note{
The cause and effect diagram is also known as "Ishikawa diagram", and
  has been widely used in Quality Management. It is one of the Seven
  Basic Tools of Quality.
}

\seealso{
\code{\link{ss.pMap}}
}


\examples{
#Data
effect<-"Flight Time"
causes.gr<-c("Operator", "Environment", "Tools", "Design", 
  "Raw.Material","Measure.Tool")
causes<-vector(mode="list", length=length(causes.gr))
causes[1]<-list(c("operator #1", "operator #2", "operator #3"))
causes[2]<-list(c("height", "cleaning"))
causes[3]<-list(c("scissors", "tape"))
causes[4]<-list(c("rotor.length", "rotor.width2", "paperclip"))
causes[5]<-list(c("thickness", "marks"))
causes[6]<-list(c("calibrate", "model"))
ss.ceDiag(effect, causes.gr, causes, sub="Paper Helicopter Project")
}
\keyword{Cause-and-Effect}
\keyword{Define}
