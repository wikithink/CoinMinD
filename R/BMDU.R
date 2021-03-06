#修改了BMDU函数的输出格式
#王文祥
#2017-12-22
BMDU <- function(x,d){
#d is the number of divisions required to split the prior vector of Dirichlet distribution to assign unequal values from U(0,1) and U(1,2)
k=length(x)
for(m in 1:k)
{
if(x[m]<0)
{cat('Warning: arguments must be non-negative integers')
}
}
if(d>=1 && d<=k)
{
m=0
l=0
u=0
diff=0
s=sum(x) 
s1=floor(k/d)
d1=runif(s1,0,1)###First half of the vector 
d2=runif(k-s1,1,2)###Second half of the vector
a=c(d1,d2)
p=x+a###Prior for Dirichlet
dr=rdirichlet(10000, p)###Posterior 
for(j in 1:k)
{
l[j]=round(quantile(dr[,j],0.025),4)###Lower Limit
u[j]=round(quantile(dr[,j],0.975),4)###Upper Limit
m[j]=round(mean(dr[,j]),4)###Point Estimate
diff[j]=u[j]-l[j]
}
VOL <- prod(diff)
# cat('Mean\n')
# print(m)
# cat('Lower Limit\n')
# print(l)
# cat('Upper Limit\n')
# print(u)
# p=prod(diff)
# cat('Volume\n')
# print(p)

}
else
{cat('warning:size of the division should be less than the size of the input matrix')
}
#转成数据框
method <- 'Dirichlet-UnEqualPrior Bayes'
bmducl <- as.data.frame(cbind(x,round(x/sum(x),4),m,l,u,method))
names(bmducl) <- c("x","prop","mean","LLA","ULA","method")
u <- list(bmducl=bmducl,vol=VOL,d=d)
return(u)
}
