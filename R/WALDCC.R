#修改了WALDCC函数的输出格式
#王文祥
#2017-12-22
WALDCC <- function(inpmat,alpha){
k = length(inpmat)
s = sum(inpmat)
chi = qchisq(1-alpha, df=1)
pi = inpmat/s
WALDCC.LL = pi - (sqrt(chi*(pi)*(1-pi)/s))-(1/(2*s)) 
WALDCC.UL = pi + (sqrt(chi*(pi)*(1-pi)/s))+(1/(2*s)) 
LLA=0
ULA=0
for (r in 1:length(inpmat))
{
if ( WALDCC.LL [r]< 0) LLA[r] = 0 else LLA[r]=WALDCC.LL[r]
if (WALDCC.UL[r] > 1) ULA[r] = 1 else ULA[r]=WALDCC.UL[r]
}
diA=ULA-LLA##FIND LENGTH OF CIs
VOL=round(prod(diA),8)##PRODUCT OF LENGTH OF CIs
# cat('Original Intervals\n')
# cat('Lower Limit\n')
# print(WALDCC.LL)
# cat('Upper Limit\n')
# print(WALDCC.UL)
# cat('Adjusted Intervals\n')
# cat('Lower Limit\n')
# print(LLA)
# cat('Upper Limit\n')
# print(ULA)
# cat('Volume\n')
# print(VOL)
#转成数据框
method <- 'Wald with continuity corrections'
waldcccl <- as.data.frame(cbind(inpmat,round(pi,4),alpha,1-alpha,WALDCC.LL,WALDCC.UL,LLA,ULA,method))
names(waldcccl) <- c("x","prop","alpha","conf","LL","UL","LLA","ULA","method")
u <- list(waldcccl=waldcccl,vol=VOL)
return(u)
}
