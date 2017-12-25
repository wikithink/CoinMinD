#修改了WS函数的输出格式
#王文祥
#2017-12-22
WS <- function(inpmat,alpha){
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=1)
pi = inpmat/s
WS.UL = (chi + 2*inpmat + sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
WS.LL = (chi + 2*inpmat - sqrt(chi*chi + 4*inpmat*chi*(1 - pi)))/(2*(chi+s))
LLA=0
ULA=0
for (r in 1:length(inpmat))
{
if ( WS.LL [r]< 0) LLA[r] = 0 else LLA[r]=WS.LL[r]
if (WS.UL[r] > 1) ULA[r] = 1 else ULA[r]=WS.UL[r]
}
diA=ULA-LLA##FIND LENGTH OF CIs
VOL=round(prod(diA),8)##PRODUCT OF LENGTH OF CIs
# cat('Original Intervals\n')
# cat('Lower Limit\n')
# print(WS.LL)
# cat('Upper Limit\n')
# print(WS.UL)
# cat('Adjusted Intervals\n')
# cat('Lower Limit\n')
# print(LLA)
# cat('Upper Limit\n')
# print(ULA)
# cat('Volume\n')
# print(VOL)
#转成数据框
method <- 'Wilson'
wscl <- as.data.frame(cbind(inpmat,round(pi,4),alpha,1-alpha,WS.LL,WS.UL,LLA,ULA,method))
names(wscl) <- c("x","prop","alpha","conf","LL","UL","LLA","ULA","method")
u <- list(wscl=wscl,vol=VOL)
return(u)
}
