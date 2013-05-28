\name{WS}
\alias{WS}
\title{
Confidence Interval -Wilson (WS)
}
\description{
 The simultaneous confidence interval for multinomial proportions based on the method proposed in Wilson (1927)
}
\usage{
WS(inpmat, alpha)
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
\references{E.B. Wilson, Probable inference, the law of succession and statistical inference, J.Am. Stat. Assoc. 22 (1927) 209-212.}
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{WALD}},\code{\link{SG}}
}
\examples{
y=c(44,55,43,32,67,78)
z=0.05
WS(y,z)
}
\keyword{ confidencelimit }