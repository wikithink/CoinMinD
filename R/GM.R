GM <-
function(inpmat,alpha)
{
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-(alpha/k), df=1)
pi = inpmat/s
GM.UL = (chi + 2*inpmat + sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
GM.LL = (chi + 2*inpmat - sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
GM.WI = GM.UL - GM.LL #Length of the interval
GM.VL = prod(GM.WI)
cat('Lower Limit\n')
print(GM.LL)
cat('Upper Limit\n')
print(GM.UL)
cat('Volume\n')
print(GM.VL)
}
