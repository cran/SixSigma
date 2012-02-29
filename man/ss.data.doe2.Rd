\name{ss.data.doe2}
\alias{ss.data.doe2}
\docType{data}
\title{
Data for the pizza dough example (robust design)
}
\description{
Experimental data for the scores given to a set of pizza doughs. Noise factors added
 for robust design.
}
\usage{data(ss.data.doe2)}
\format{
  A data frame with 64 observations on the following 7 variables.
  \describe{
    \item{\code{repl}}{Replication id}
    \item{\code{flour}}{Level of flour in the recipe (\code{-} \code{+})}
    \item{\code{salt}}{Level of salt in the recipe (\code{-} \code{+})}
    \item{\code{bakPow}}{Level of Baking Powder in the recipe (\code{-} \code{+})}
    \item{\code{temp}}{Level of temperature in preparation (\code{-} \code{+})}
    \item{\code{time}}{Level of time in preparation (\code{-} \code{+})}
    \item{\code{score}}{Scored assigned to the recipe}
  }
}
\details{
This data are used for the examples in Chapter 11 of the book ``Six Sigma with R''.
}
\source{
E. L. Cano, J. M. Moguerza, and A. Redchuk,
 \emph{Six Sigma with R. Statistical Engineering for Process
 Improvement}, volume 36 of \emph{Use R!}, Springer, New York, 2012.
 ISBN 978-1-4614-3651-5.
 \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.
}
\references{
E. L. Cano, J. M. Moguerza, and A. Redchuk,
 \emph{Six Sigma with R. Statistical Engineering for Process
 Improvement}, volume 36 of \emph{Use R!}, Springer, New York, 2012.
 ISBN 978-1-4614-3651-5.
 \url{http://www.springer.com/statistics/book/978-1-4614-3651-5}.
}
\examples{
data(ss.data.doe2)
}
\keyword{datasets}
