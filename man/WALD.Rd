\name{WALD}
\alias{WALD}
\title{
Confidence Interval -WALD
}
\description{
The simple Wald type interval for multinomial proportions which is symmetrical about the sample proportions. In this method no continuity corrections are made to avoid zero width intervals when the sample proportions are at extreme.}
\usage{
WALD(inpmat, alpha)
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
\references{Wald, A Tests of statistical hypotheses concerning several parameters when the number of observations is large, Trans. Am. Math. Soc. 54 (1943) 426-482.
}
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALDCC}},\code{\link{SG}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
WALD(y,z)
}
\keyword{ confidencelimit }