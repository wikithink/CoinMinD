\name{BMDE}
\alias{BMDE}
\title{
Multinomial - Dirichlet (MD) model - Equal Prior - Bayes Methods
}
\description{
This method provides 95 percent simultaneous confidence interval for multinomial proportions based on Bayesian Multinomial Dirichlet model. This method assumes equal values for the Dirichlet prior parameters}
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
\references{
Gelman, A., Carlin, J.B., Stern, H.S., and Rubin, D.B. (2002). Bayesian Data Analysis.  Chapman & Hall, London.
}
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
}
\keyword{ Bayes Method }