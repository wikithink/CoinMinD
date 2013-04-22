\name{GM}
\alias{GM}
\title{
Confidence Interval - Goodman
}
\description{
Goodman (GM) differ from Queensberry and Hurst only in ??^2 values; A and A2 instead of A1 in QH. 
}
\usage{
GM(inpmat, alpha)
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
GM(y,z)
function (inpmat, alpha) 
{
    k = length(inpmat)
    s = sum(inpmat)
    chi = qchisq(1 - (alpha/k), df = 1)
    pi = inpmat/s
    GM.UL = (chi + 2 * inpmat + sqrt(chi * chi + 4 * inpmat * 
        chi * (1 - pi)))/(2 * (chi + s))
    GM.LL = (chi + 2 * inpmat - sqrt(chi * chi + 4 * inpmat * 
        chi * (1 - pi)))/(2 * (chi + s))
    GM.WI = GM.UL - GM.LL
    GM.VL = prod(GM.WI)
    cat("Lower Limit\n")
    print(GM.LL)
    cat("Upper Limit\n")
    print(GM.UL)
    cat("Volume\n")
    print(GM.VL)
  }
}
\keyword{ Confidenceinterval }
