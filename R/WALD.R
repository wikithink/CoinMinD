WALD <-
function(inpmat,alpha)
{
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=1)
pi = inpmat/s
WALD.LL = pi - (sqrt(chi*(pi)*(1-pi)/s)) 
WALD.UL = pi + (sqrt(chi*(pi)*(1-pi)/s)) 
WALD.WI = WALD.UL-WALD.LL
WALD.VL = prod(WALD.WI)
cat('Lower Limit\n')
print(WALD.LL)
cat('Upper Limit\n')
print(WALD.UL)
cat('Volume\n')
print(WALD.VL)
}
