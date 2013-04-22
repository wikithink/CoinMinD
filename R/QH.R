QH <-
function(inpmat,alpha)
{k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=k-1)
pi = inpmat/s
QH.UL = (chi + 2*inpmat + sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
QH.LL = (chi + 2*inpmat - sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
QH.WI = QH.UL - QH.LL #Length of the interval
QH.VL = prod(QH.WI)
cat('Lower Limit\n')
print(QH.LL)
cat('Upper Limit\n')
print(QH.UL)
cat('Volume\n')
print(QH.VL)
}
