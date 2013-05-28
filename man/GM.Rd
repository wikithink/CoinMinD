\name{GM}
\alias{GM}
\title{
Confidence Interval - Goodman
}
\description{
The simultaneous confidence interval for multinomial proportions based on the method proposed in Goodman (1965)}
\usage{
GM(inpmat, alpha)
}
\arguments{
  \item{inpmat}{
inpmat refers to the cell counts of given contingency table corresponding to a categorical data}
  \item{alpha}{
a number between 0 and 1 to get the upper 100(1-??) percentage point of the chi square distribution
}
}
\value{
lower, upper limits of multinomial proportions together with product of length of k intervals as volume of simultaneous confidence intervals }
\references{Goodman, L.A. (1965). On Simultaneous Confidence Intervals for Multinomial Proportions. Technometrics 7:  247-254.}
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALD}},\code{\link{WS}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
GM(y,z)
}
\keyword{ Confidenceinterval }