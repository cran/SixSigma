% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qcr.profiles.R
\name{plotProfiles}
\alias{plotProfiles}
\title{Plot Profiles}
\usage{
plotProfiles(
  profiles,
  x = 1:nrow(profiles),
  cLimits = NULL,
  outControl = NULL,
  onlyout = FALSE
)
}
\arguments{
\item{profiles}{matrix with profiles in columns}

\item{x}{vector with the independent variable}

\item{cLimits}{matrix with three profiles: prototype and confidence bands (limits)}

\item{outControl}{identifiers of out-of-control profiles}

\item{onlyout}{plot only out-of-control profiles? [FALSE]}
}
\value{
Only graphical output with the profiles
}
\description{
Plot profiles and optionally control limits
}
\examples{
plotProfiles(profiles = ss.data.wby,
    x = ss.data.wbx)     
}
\references{
Cano, E.L. and Moguerza, J.M. and Prieto Corcoba, M. (2015)
\emph{Quality Control with R. An ISO Standards Approach}. Springer.
}
\author{
Javier M. Moguerza and Emilio L. Cano
}
