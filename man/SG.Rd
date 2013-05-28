\name{SG}
\alias{SG}
\title{
Confidence Interval -Sison and Glaz
}
\description{
The simultaneous confidence interval for multinomial proportions based on the method proposed in Sison and Glaz (1995).}
\usage{
SG(x, alpha)
}
\arguments{
 \item{x}{
x refers to the cell counts of given contingency table corresponding to a categorical data}
  \item{alpha}{
a number between 0 and 1 to get the upper 100(1-??) percentage point of the chi square distribution
}
}
\value{
lower, upper limits of multinomial proportions together with product of length of k intervals as volume of simultaneous confidence intervals }
\references{Sison, P.C. and Glaz J. (1995). Simultaneous Confidence Intervals and Sample Size Determination for Multinomial Proportions. Journal of the American Statistical Association 90: 366-369.}
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALD}},\code{\link{GM}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
SG(y,z)
}
\keyword{ confidencelimit }