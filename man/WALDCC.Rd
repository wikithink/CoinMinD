\name{WALDCC}
\alias{WALDCC}
\title{
Confidence Interval -WALDCC
}
\description{
The simple Wald type interval with continuity corrections for multinomial proportions which is symmetrical about the sample proportions.}
\usage{
WALDCC(inpmat, alpha)
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
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALD}},\code{\link{SG}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
WALDCC(y,z)
}
\keyword{ confidencelimit }