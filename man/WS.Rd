\name{WS}
\alias{WS}
\title{
Confidence Interval -Wilson (WS)
}
\description{
 Wilson (WS) differ from Queensberry and Hurst only in ??^2 values; A and A2 instead of A1 in QH. 
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
function (inpmat, alpha) 
{
    k = length(inpmat)
    s = sum(inpmat)
    chi = qchisq(1 - alpha, df = 1)
    pi = inpmat/s
    WS.UL = (chi + 2 * inpmat + sqrt(chi * chi + 4 * inpmat * 
        chi * (1 - pi)))/(2 * (chi + s))
    WS.LL = (chi + 2 * inpmat - sqrt(chi * chi + 4 * inpmat * 
        chi * (1 - pi)))/(2 * (chi + s))
    WS.WI = WS.UL - WS.LL
    WS.VL = prod(WS.WI)
    WS.VL = prod(WS.WI)
    cat("Lower Limit\n")
    print(WS.LL)
    cat("Upper Limit\n")
    print(WS.UL)
    cat("Volume\n")
    print(WS.VL)
  }
}
\keyword{ confidencelimit }
