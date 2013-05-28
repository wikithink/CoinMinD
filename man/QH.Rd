\name{QH}
\alias{QH}
\title{
Confidence Interval -Quesenberry and Hurst
}
\description{
The simultaneous confidence interval for multinomial proportions based on the method proposed in Quesenberry and Hurst (1964)}
\usage{
QH(inpmat, alpha)
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
\references{
Quesensberry, C.P. and Hurst, D.C. (1964). Large Sample Simultaneous Confidence Intervals for Multinational Proportions. Technometrics, 6: 191-195.}
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALD}},\code{\link{WS}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
QH(y,z)
}
\keyword{ confidencelimit }