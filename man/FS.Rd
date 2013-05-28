\name{FS}
\alias{FS}
\title{
Confidence Interval - Fitzpatrick and Scott
}
\description{
The simultaneous confidence interval for multinomial proportions based on the method proposed in Fitzpatrick and Scott (1987)
}
\usage{
FS(inpmat, alpha)
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
Fitzpatrick, S. and Scott, A. (1987). Quick simultaneous confidence interval for multinomial proportions. Journal of American Statistical Association 82(399):  875-878.
}
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALD}},\code{\link{WS}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
FS(y,z)
}
\keyword{ Confidencelimit }