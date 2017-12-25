#修改了BMDE函数的输出格式
#王文祥
#2017-12-22
BMDE <-function(x,p){
#p is the equal value for the Dirichlet prior parameter - positive real number
k=length(x)
n_r=10000
for(m in 1:k)
{
if(x[m]<0)
{cat('Warning: arguments must be non-negative integers')
}
}

po=x+p
dr=rdirichlet(n_r,po)
a=0
l=0
u=0
dif=0 
for(j in 1:k)
{
a[j]=round(mean(dr[,j]),4)
l[j]=round(quantile(dr[,j],0.025),4)
u[j]=round(quantile(dr[,j],0.975),4)
dif[j]=u[j]-l[j]
}
VOL <- prod(dif)
# cat('Mean\n')
# print(a)
# cat('Lower Limit\n')
# print(l)
# cat('Upper Limit\n')
# print(u)
# cat('Volume \n')
# prod(dif)
#转成数据框
method <- 'Dirichlet-EqualPrior Bayes'
bmdecl <- as.data.frame(cbind(x,round(x/sum(x),4),a,l,u,method))
names(bmdecl) <- c("x","prop","mean","LLA","ULA","method")
u <- list(bmdecl=bmdecl,vol=VOL,p=p)
return(u)
}
