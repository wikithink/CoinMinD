WALDCC <-
function(inpmat,alpha)
{
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=1)
pi = inpmat/s
WALDCC.LL = pi - (sqrt(chi*(pi)*(1-pi)/s))-(1/(2*s)) 
WALDCC.UL = pi + (sqrt(chi*(pi)*(1-pi)/s))+(1/(2*s)) 
WALDCC.WI=WALDCC.UL-WALDCC.LL
WALDCC.VL = prod(WALDCC.WI)
cat('Lower Limit\n')
print(WALDCC.LL)
cat('Upper Limit\n')
print(WALDCC.UL)
cat('Volume\n')
print(WALDCC.VL)
}
