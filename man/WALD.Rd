\name{WALD}
\alias{WALD}
\title{
Confidence Interval -WALD
}
\description{
In classical approach, WALD has proposed confidence interval Without continuity correction
}
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
function (inpmat, alpha) 
{
    k = length(inpmat)
    s = sum(inpmat)
    chi = qchisq(1 - alpha, df = 1)
    pi = inpmat/s
    WALD.LL = pi - (sqrt(chi * (pi) * (1 - pi)/s))
    WALD.UL = pi + (sqrt(chi * (pi) * (1 - pi)/s))
    WALD.WI = WALD.UL - WALD.LL
    WALD.VL = prod(WALD.WI)
    cat("Lower Limit\n")
    print(WALD.LL)
    cat("Upper Limit\n")
    print(WALD.UL)
    cat("Volume\n")
    print(WALD.VL)
  }
}
\keyword{ confidencelimit }

