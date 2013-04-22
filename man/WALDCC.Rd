\name{WALDCC}
\alias{WALDCC}
\title{
Confidence Interval -WALDCC
}
\description{
In classical approach, WALD has proposed confidence interval With continuity correction
}
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
function (inpmat, alpha) 
{
    k = length(inpmat)
    s = sum(inpmat)
    chi = qchisq(1 - alpha, df = 1)
    pi = inpmat/s
    WALDCC.LL = pi - (sqrt(chi * (pi) * (1 - pi)/s)) - (1/(2 * 
        s))
    WALDCC.UL = pi + (sqrt(chi * (pi) * (1 - pi)/s)) + (1/(2 * 
        s))
    WALDCC.WI = WALDCC.UL - WALDCC.LL
    WALDCC.VL = prod(WALDCC.WI)
    cat("Lower Limit\n")
    print(WALDCC.LL)
    cat("Upper Limit\n")
    print(WALDCC.UL)
    cat("Volume\n")
    print(WALDCC.VL)
  }
}
\keyword{ confidencelimit }

