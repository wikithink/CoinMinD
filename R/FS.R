FS <-
function(inpmat,alpha)
{
k = length(inpmat)
s = sum(inpmat)
zval = abs(qnorm(1-(alpha/2)))
pi = inpmat/s
FS.LL = pi - (zval/(2*sqrt(s))) 
FS.UL = pi + (zval/(2*sqrt(s))) 
FS.WI = FS.UL-FS.LL
FS.VL = prod(FS.WI)
cat('Lower Limit\n')
print(FS.LL)
cat('Upper Limit\n')
print(FS.UL)
cat('Volume\n')
print(FS.VL)
}
