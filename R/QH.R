#修改了QH函数的输出格式
#王文祥
#2017-12-22
QH <- function(inpmat,alpha){
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=k-1)
pi = inpmat/s
QH.UL = (chi + 2*inpmat + sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
QH.LL = (chi + 2*inpmat - sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
LLA=0
ULA=0
for (r in 1:length(inpmat))
{
if (QH.LL [r]< 0) LLA[r] = 0 else LLA[r]=QH.LL[r]
if (QH.UL[r] > 1) ULA[r] = 1 else ULA[r]=QH.UL[r]
}
diA=ULA-LLA##FIND LENGTH OF CIs
VOL=round(prod(diA),8)##PRODUCT OF LENGTH OF CIs
# cat('Original Intervals\n')
# cat('Lower Limit\n')
# print(QH.LL)
# cat('Upper Limit\n')
# print(QH.UL)
# cat('Adjusted Intervals\n')
# cat('Lower Limit\n')
# print(LLA)
# cat('Upper Limit\n')
# print(ULA)
# cat('Volume\n')
# print(VOL)
#转成数据框
method <- 'Quesenberry and Hurst'
qhcl <- as.data.frame(cbind(inpmat,round(pi,4),alpha,1-alpha,QH.LL,QH.UL,LLA,ULA,method))
names(qhcl) <- c("x","prop","alpha","conf","LL","UL","LLA","ULA","method")
u <- list(qhcl=qhcl,vol=VOL)
return(u)
}
