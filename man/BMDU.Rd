\name{BMDU}
\alias{BMDU}
\title{
Multinomial - Dirichlet (MD) model - UnEqual Prior - Bayes Methods
}
\description{
This method provides 95 percent simultaneous confidence interval for multinomial proportions based on Bayesian Multinomial Dirichlet model. However, it provides a mechanism through which user can split the Dirichlet prior parameter vector and suitable distributions can be incorporated for each of two groups.}
\usage{
BMDU(x, d)
}
\arguments{
  \item{x}{
x refers to the cell counts of given contingency table corresponding to a categorical data - non negative integers
}
  \item{d}{
d is the number of divisions required to split the prior vector of Dirichlet distribution to assign unequal values from U(0,1) and U(1,2)
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
}
\keyword{ Confidencelimit }
