\name{QH}
\alias{QH}
\title{
Confidence Interval -Quesenberry and Hurst
}
\description{
In classical approach, Quesenberry and Hurst (1964), have proposed confidence interval (QH) based on Pearson statistic
}
\usage{
QH(inpmat, alpha)
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
QH(y,z)
function (inpmat, alpha) 
{
    k = length(inpmat)
    s = sum(inpmat)
    chi = qchisq(1 - alpha, df = k - 1)
    pi = inpmat/s
    QH.UL = (chi + 2 * inpmat + sqrt(chi * chi + 4 * inpmat * 
        chi * (1 - pi)))/(2 * (chi + s))
    QH.LL = (chi + 2 * inpmat - sqrt(chi * chi + 4 * inpmat * 
        chi * (1 - pi)))/(2 * (chi + s))
    QH.WI = QH.UL - QH.LL
    QH.VL = prod(QH.WI)
    cat("Lower Limit\n")
    print(QH.LL)
    cat("Upper Limit\n")
    print(QH.UL)
    cat("Volume\n")
    print(QH.VL)
  }
}
\keyword{ confidencelimit }

