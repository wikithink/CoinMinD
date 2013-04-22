\name{FS}
\alias{FS}
\title{
Confidence Interval - Fitzpatrick and Scott
}
\description{
Fitzpatrick and Scott (1987) have proposed confidence interval (FS)
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
function (inpmat, alpha) 
{
    k = length(inpmat)
    s = sum(inpmat)
    zval = abs(qnorm(1 - (alpha/2)))
    pi = inpmat/s
    FS.LL = pi - (zval/(2 * sqrt(s)))
    FS.UL = pi + (zval/(2 * sqrt(s)))
    FS.WI = FS.UL - FS.LL
    FS.VL = prod(FS.WI)
    cat("Lower Limit\n")
    print(FS.LL)
    cat("Upper Limit\n")
    print(FS.UL)
    cat("Volume\n")
    print(FS.VL)
  }
}
\keyword{ Confidencelimit }
