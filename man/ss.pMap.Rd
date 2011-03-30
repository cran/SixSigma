\name{ss.pMap}
\alias{ss.pMap}

\title{
Process Map
}
\description{
This function takes information about the process we want
  to represent and draw the Map Process, with its X's, x's, Y's and y's}
\usage{
ss.pMap(procs, inputs.overall, outputs.overall, input.output,
x.parameters, y.features, main = "Six Sigma Process Map", sub, ss.col =
c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"))
}

\arguments{
  \item{procs}{
A vector of character with the name of the 'n' processes
}
  \item{inputs.overall}{
A vector of characters with the name of the overall inputs
}
  \item{outputs.overall}{
A vector of characters with the name of the overall outputs
}
  \item{input.output}{
A vector of lists with the names of the inputs of the \eqn{i-{th}} process,
  that will be the outputs of the \eqn{(i-1)-{th}} process
}
  \item{x.parameters}{
A vector of lists with a list of the x parameters of the process. The
  parameter is a vector with two values: the name and the type (view details)
}
\item{y.features}{
  A vector of lists with a list of the y features of the process. The
  feature is a vector with two values: the name and the type (view details)
}
  \item{main}{
The main title for the Process Map
}
\item{sub}{
  Subtitle for the diagram (recommended the Six Sigma project name)
}
\item{ss.col}{
  A vector of colours for a custom drawing. At least five colours,
  sorted by descendant intensity (see details)
}
}
\details{
The type of the x parameters and y features can be: C(controllable), 
N(noise), Cr(Critical), P(Procedure). The default value for ss.col is 
c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), a grayscale style. 
You can pass any accepted color string.
}
\value{
A graphic representation of the Map Process.
}
\references{
  \url{http://en.wikipedia.org/wiki/Business_Process_Mapping}
}
\author{
Emilio Lopez
}
\note{
The process map is the starting point for a Six Sigma Project, and it is
  very important to find out who the x's and y'x are.
}

\seealso{
  \code{\link{ss.ceDiag}}
}
\examples{
inputs.overall<-c("operators", "tools", "raw material", "facilities")
outputs.overall<-c("helicopter")
procs<-c("INSPECTION", "ASSEMBLY", "TEST", "LABELING")
#Inputs of process "i" are inputs of process "i+1"
input.output<-vector(mode="list",length=length(procs))
input.output[1]<-list(c("sheets", "..."))
input.output[2]<-list(c("sheets"))
input.output[3]<-list(c("helicopter"))
input.output[4]<-list(c("helicopter"))
input.output[[1]][2]

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
x.parameters

#Features of each process
y.features<-vector(mode="list",length=length(procs))
y.features[1]<-list(c(list(c("ok", "Cr"))))
y.features[2]<-list(c(list(c("weight", "Cr"))))
y.features[3]<-list(c(list(c("time", "Cr"))))
y.features[4]<-list(c(list(c("label", "Cr"))))
y.features

ss.pMap(procs, inputs.overall, outputs.overall,
        input.output, x.parameters, y.features, 
        sub="Paper Helicopter Project")
}
\keyword{ Ishikawa }
\keyword{ cause-and-effect }
\keyword{ Define }
