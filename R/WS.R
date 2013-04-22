WS <-
function(inpmat,alpha)
{
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=1)
pi = inpmat/s
WS.UL = (chi + 2*inpmat + sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
WS.LL = (chi + 2*inpmat - sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
WS.WI = WS.UL-WS.LL
WS.VL = prod(WS.WI)
WS.VL = prod(WS.WI)
cat('Lower Limit\n')
print(WS.LL)
cat('Upper Limit\n')
print(WS.UL)
cat('Volume\n')
print(WS.VL)
}
