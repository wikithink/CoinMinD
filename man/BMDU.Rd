\name{BMDU}
\alias{BMDU}
\title{
Multinomial - Dirichlet (MD) model - UnEqual Prior - Bayes Methods
}
\description{
This method provides a mechanism through which analyst can split the parameter vector (??1, ??2, ??3,.. ??k) and suitable prior distributions can be incorporated for each of two groups.
}
\usage{
BMDU(x, d)
}
\arguments{
  \item{x}{
x refers to the cell counts of given contingency table corresponding to a categorical data - non negative integers
}
  \item{d}{
d is the number of divisions required for split the number of cells to assign unequal priors for the Dirichlet distribution
}
}
\value{
lower, upper limits of multinomial proportions together with product of length of k intervals as volume of simultaneous confidence intervals }
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDE}},\code{\link{GM}},\code{\link{WS}}
}
\examples{
y=c(44,55,43,32,67,78)
z=2
BMDU(y,z)
function (x, d) 
{
    k = length(x)
    for (m in 1:k) {
        if (x[m] < 0) {
            cat("Warning: arguments must be non-negative integers")
        }
    }
    if (d >= 1 && d <= k) {
        m = 0
        l = 0
        u = 0
        diff = 0
        s = sum(x)
        s1 = floor(k/d)
        d1 = runif(s1, 0, 1)
        d2 = runif(k - s1, 1, 2)
        a = c(d1, d2)
        p = x + a
        dr = rdirichlet(10000, p)
        for (j in 1:k) {
            l[j] = round(quantile(dr[, j], 0.025), 4)
            u[j] = round(quantile(dr[, j], 0.975), 4)
            m[j] = round(mean(dr[, j]), 4)
            diff[j] = u[j] - l[j]
        }
        cat("Mean\n")
        print(m)
        cat("Lower Limit\n")
        print(l)
        cat("Upper Limit\n")
        print(u)
        p = prod(diff)
        cat("Volume\n")
        print(p)
    }
    else {
        cat("warning:size of the division should be less than the size of the input matrix")
    }
  }
}
\keyword{ Confidencelimit }

