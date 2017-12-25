#修改了FS函数的输出格式,增加了单双侧参数alter
#alter 1为单侧，2为双侧
#王文祥
#2017-12-22
FS <- function(inpmat,alpha,alter){
k = length(inpmat)
s = sum(inpmat)
#增加单双侧的参数
if(alter==1){
  zval = abs(qnorm(1-alpha)) 
}else if(alter==2){
  zval = abs(qnorm(1-(alpha/2))) 
}else{
  zval = abs(qnorm(1-(alpha/2)))
}

pi = inpmat/s
FS.LL = pi - (zval/(2*sqrt(s))) 
FS.UL = pi + (zval/(2*sqrt(s))) 
LLA=0
ULA=0
for (r in 1:length(inpmat))
{
if ( FS.LL [r]< 0) LLA[r] = 0 else LLA[r]=FS.LL[r]
if (FS.UL[r] > 1) ULA[r] = 1 else ULA[r]=FS.UL[r]
}
diA <- ULA-LLA##FIND LENGTH OF CIs
VOL <- round(prod(diA),8)##PRODUCT OF LENGTH OF CIs
# cat('Original Intervals\n')
# cat('Lower Limit\n')
# print(FS.LL)
# cat('Upper Limit\n')
# print(FS.UL)
# cat('Adjusted Intervals\n')
# cat('Lower Limit\n')
# print(LLA)
# cat('Upper Limit\n')
# print(ULA)
# cat('Volume\n')
# print(VOL)
#转成数据框
method <- 'Fitzpatrick and Scott'
fscl <- as.data.frame(cbind(inpmat,round(pi,4),alpha,1-alpha,FS.LL,FS.UL,LLA,ULA,method))
names(fscl) <- c("x","prop","alpha","conf","LL","UL","LLA","ULA","method")
u <- list(fscl=fscl,vol=VOL)
return(u)
}
