\name{BMDE}
\alias{BMDE}
\title{
Multinomial - Dirichlet (MD) model - Equal Prior - Bayes Methods
}
\description{
This method provides a mechanism to find out simultaneous confidence interval for the counts of a two way contingency table. The method bmde assumes equal values for ??'s
}
\usage{
BMDE(x, p)
}
\arguments{
  \item{x}{
x refers to the cell counts of given contingency table corresponding to a categorical data - non negative integers
}
  \item{p}{
the equal value for the Dirichlet prior parameter - positive real number
}
}
\value{
lower, upper limits of multinomial proportions together with product of length of k intervals as volume of simultaneous confidence intervals }
\author{
Dr M Subbiah
}
\seealso{
\code{\link{BMDU}},\code{\link{FS}},\code{\link{QH}}
}
\examples{
y=c(44,55,43,32,67,78)
z=1
BMDE(y,z)
function (x, p) 
{
    k = length(x)
    n_r = 10000
    for (m in 1:k) {
        if (x[m] < 0) {
            cat("Warning: arguments must be non-negative integers")
        }
    }
    po = x + p
    dr = rdirichlet(n_r, po)
    a = 0
    l = 0
    u = 0
    dif = 0
    for (j in 1:k) {
        a[j] = round(mean(dr[, j]), 4)
        l[j] = round(quantile(dr[, j], 0.025), 4)
        u[j] = round(quantile(dr[, j], 0.975), 4)
        dif[j] = u[j] - l[j]
    }
    cat("Mean\n")
    print(a)
    cat("Lower Limit\n")
    print(l)
    cat("Upper Limit\n")
    print(u)
    cat("Volume \n")
    prod(dif)
  }
}
\keyword{ Bayes Method }

