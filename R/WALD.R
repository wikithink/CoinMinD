#修改了WALD函数的输出格式
#王文祥
#2017-12-22
WALD <- function(inpmat,alpha){
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=1)
pi = inpmat/s
WALD.LL = pi - (sqrt(chi*(pi)*(1-pi)/s)) 
WALD.UL = pi + (sqrt(chi*(pi)*(1-pi)/s)) 
LLA=0
ULA=0
for (r in 1:length(inpmat))
{
if ( WALD.LL [r]< 0) LLA[r] = 0 else LLA[r]=WALD.LL[r]
if (WALD.UL[r] > 1) ULA[r] = 1 else ULA[r]=WALD.UL[r]
}
diA=ULA-LLA##FIND LENGTH OF CIs
VOL=round(prod(diA),8)##PRODUCT OF LENGTH OF CIs
# cat('Original Intervals\n')
# cat('Lower Limit\n')
# print(WALD.LL)
# cat('Upper Limit\n')
# print(WALD.UL)
# cat('Adjusted Intervals\n')
# cat('Lower Limit\n')
# print(LLA)
# cat('Upper Limit\n')
# print(ULA)
# cat('Volume\n')
# print(VOL)
#转成数据框
method <- 'Wald'
waldcl <- as.data.frame(cbind(inpmat,round(pi,4),alpha,1-alpha,WALD.LL,WALD.UL,LLA,ULA,method))
names(waldcl) <- c("x","prop","alpha","conf","LL","UL","LLA","ULA","method")
u <- list(waldcl=waldcl,vol=VOL)
return(u)
}
