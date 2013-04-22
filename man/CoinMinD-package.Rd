\name{CoinMinD-package}
\alias{CoinMinD-package}
\alias{CoinMinD}
\docType{package}
\title{
Confidence Interval for Multinomial Proportion - CoinMinD
}
\description{
Methods for obtaining simultaneous confidence interval for multinomial proportion have been proposed by many authors and the present study include a variety of widely applicable procedures. Seven classical methods (Wilson, Quesenberry and Hurst, Goodman, Wald with and without continuity correction, Fitzpatrick and Scott, Sison and Glaz) and Bayesian Dirichlet models are included in the package. The advantage of MCMC pack has been exploited to derive the Dirichlet posterior directly and this also helps in handling the Dirichlet prior parameters. This package is prepared to have equal and unequal values for the Dirichlet prior distribution that will provide better scope for data analysis and associated sensitivity analysis. 
}
\details{
\tabular{ll}{
Package: \tab CoinMinD\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-04-22\cr
License: \tab GPL-2\cr
}
}
\author{
Dr M Subbiah
Maintainer: Sumathi<sumathimr@yahoo.co.in>
}
\references{
1.  Fitzpatrick, S. and Scott, A. (1987). Quick simultaneous confidence interval for multinomial proportions. Journal of American Statistical Association 82(399):  875-878.
2.	Glaz, J. and Sison, P.C. (1999). Simultaneous confidence interval for multinomial proportions. Journal of Statistical planning and inference 82:  251-262.
3.	Goodman, L.A. (1965). On Simultaneous Confidence Intervals for Multinomial Proportions. Technometrics 7:  247-254.
4.	Hou, C.D, Chiang, J. and Tai, J.J. (2003). A family of simultaneous confidence intervals for multinomial proportions. Computational Statistics & Data Analysis 43: 29-45.
5.	Jhun, M. and Jeong, H.C. (2000). Applications of bootstrap methods for categorical data analysis. Computational Statistics & Data Analysis 35:  83-91.
6.	May L.W. and Johnson D.W. (1997). Constructing simultaneous confidence intervals for multinomial proportions. Computer Methods and Programs in Biomedicine 53: 153-162.
7.	Quesenberry, C.P. and Hurst, D.C. (1964). Large Sample Simultaneous Confidence Intervals for Multinational Proportions. Technometrics, 6: 191-195.
8.	Sison, P.C. and Glaz J. (1995). Simultaneous Confidence Intervals and Sample Size Determination for Multinomial Proportions. Journal of the American Statistical Association 90: 366-369.
9.	Sangeetha, U Subbiah, M and Srinivasan M R (2013). Simultaneous confidence intervals for Multinomial proportions of sparse contingency tables. Communicated to Communications in Statistics - Simulation and Computation
10.	Wang, H. (2008). Exact confidence coefficients of simultaneous confidence intervals for 
multinomial proportions. Journal of Multivariate Analysis 99: 896-911.
}
\keyword{ package }
\seealso{
\code{\link{BMDE}},\code{\link{BMDU}},\code{\link{QH}}
}
\examples{
y=c(44,55,43,32,67,78)
z=1
BMDE(y,z)
}
