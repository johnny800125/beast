library(stats)
library(readxl)
data = read_excel("C:/Users/user/Desktop/論文變數/data2.xlsx")
data = data[,-1]

data = ts(data = data,start = c(2005,1),end = c(2018,12),frequency = 12)
dif.data = log(data[,1:6])%>%diff()
m1<-filter(dif.data[,6],filter=c(rep(1/12,12)),sides = 1)
dif.data = dif.data[-1:-13,]
dif.data = matrix(c(dif.data[,1:5],m1[-1:-13]),ncol=6)
dif.data = as.data.frame(dif.data)
dif.data2 = matrix(c(dif.data[54:154,1],dif.data[53:153,3],dif.data[53:153,5],dif.data[42:142,6]),ncol=4)
dif.data2 = as.data.frame(dif.data2)
reg = lm(dif.data2$V1~ dif.data2$V2+dif.data2$V3+dif.data2$V4,data = dif.data2)
summary(reg)
r = reg$residuals
r = ts(r,start = c(2009,2),frequency = 12)
plot(r,type = "l")
I <- function(psi){
  if(psi[1] > 0 && psi[2]>=0 && psi[3]>=0 && psi[2]+psi[3]<1
     
     && psi[4]>-1 && psi[4]<1) 
      {return(1)}
  
  else {return(0)} 
}

fn = function(psi,r){
  a0 = psi[1]
  a1 = psi[2]
  b1 = psi[3]
  mu = psi[4] ##mean(r) ##In case we want to omit mu as parameter
  n = length(r)
  v = rep(0,n)
  A = rep(0,n)
  v[1] = r[1]^2
  A[1] = r[1]-mu
  sum = log(v[1]) + A[1]^2/v[1]
    for (i in 2:n){
     A[i] = r[i]-mu
      v[i] = a0 + a1*A[i-1]^2 + b1*v[i-1]
      sum = sum + log(v[i]) + (A[i]^2/v[i])
  }
  return(-sum)
}

LI <- function(psi,r){
  return(fn(psi,r)*I(psi))
}

parinit = c(0.1,0.1,0.1,0.1)
opt = optim(parinit,LI,r=r,method="SANN",
       control=list(fnscale=-1,maxit=100000))

opt$par ##omega/alpha/beta/mu

Vl = sqrt(opt$par[1]/(1-opt$par[2]-opt$par[3]))
Vl ####long-term volatility
##################套件
library(fBasics)
library(fGarch)
r = as.numeric(r)
m2=garchFit(~garch(1,1),data=boxcox,trace=F,cond.dist="sstd")
summary(m2)
plot(m2)
parr = as.numeric(m2@fit$par)
Vll = sqrt(parr[2]/(1-parr[3]-parr[4]))
Vll ############# long term volatility

library(rugarch)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(3,1),arfima = FALSE),distribution.model = 'std')
model.norm1 = ugarchfit(spec = spec, data = boxcox,solver = 'hybrid' )
par(mfrow = c(1,1))
plot(model.norm1,which = 1)
par(mfrow = c(2,1))
plot(model.norm1,which = 9)


