load("posterior.image")
library(abind)

xnames<-c("deg","age","sex","child","yvar")
xlnames<-list();for(k in 1:m){xlnames[[k]]<-paste(xnames[k],c(1:K[k]),sep="") }
xlnames[[m+1]]<-c("words","tv")
xlnames[[1]]<-c("deg.1","deg.2","deg.3","deg.4")
xlnames[[2]]<-c("age.1","age.2","age.3","age.4")
xlnames[[3]]<-c("sex.m","sex.f")
xlnames[[4]]<-c("child.0","child.1","child.2","child.3")

tmp<-manova(Y~as.factor(X[,1])+as.factor(X[,2])+as.factor(X[,3])+as.factor(X[,4]))

yfit<-tmp$fitted
ymfit1<-tapply(yfit[,1],list(X[,1],X[,2],X[,3],X[,4]),mean)
ymfit2<-tapply(yfit[,2],list(X[,1],X[,2],X[,3],X[,4]),mean)
mean((Ybar[,,,,1] - EBps[,,,,1]/nss)^2,na.rm=TRUE) # .2857  / .28721
mean((Ybar[,,,,2] - EBps[,,,,2]/nss)^2,na.rm=TRUE) # .2843 / .28887

# for hadd
# 0.326
# 0.284

mean( (ymfit1-Ybar[,,,,1])^2,na.rm=TRUE) # .328
mean( (ymfit2-Ybar[,,,,2])^2,na.rm=TRUE) # .291

mean( (ymfit1-Ybar[,,,,1])^2,na.rm=TRUE)/
mean((Ybar[,,,,1] - EBps[,,,,1]/nss)^2,na.rm=TRUE)  

mean( (ymfit2-Ybar[,,,,2])^2,na.rm=TRUE)/
mean((Ybar[,,,,2] - EBps[,,,,2]/nss)^2,na.rm=TRUE) 


### data plots
pdf("wtv_data.pdf",height=4,width=7,family="Times")
xlevs<-list() 
xlevs[[1]]<-xlevs[[2]]<-1:4 ; xlevs[[3]]<-c("male","female") ;xlevs[[4]]<- 0:3
par(mfrow=c(p,m),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
for(j in 1:p){ for(k in 1:m){  
  boxplot(Y[,j]~X[,k],xaxt="n",col="gray")  
  axis(1,at=1:K[k],xlevs[[k]]) 
  if(k==1) {  mtext(xlnames[[m+1]][j],2,line=1.75,cex=.7) } 
  if(j==p) {  mtext(xnames[k],1,line=1.75,cex=.7) }
}}
dev.off()


###


###  posterior analysis plots
set.seed(1)
par(mfrow=c(1,2))
UW.pm<-UWps/nss
UW.hat<-U.als(UW.pm,R,tol=1e-10)
scl<-t(sapply(UW.hat,function(x){ apply(x^2,2,mean) } ))
mscl<-apply(scl,2,prod)^(1/(m+1))
for(k in 1:(m+1)){ UW.hat[[k]]<-t(t(UW.hat[[k]])*(sqrt(mscl/scl[k,])) ) }
#plot(UW.pm, M.U(UW.hat)) ; abline(0,1)


if(k0>1)
{
  UW.hat[[4]][,1]<- -1*UW.hat[[4]][,1] ; UW.hat[[2]][,1]<- -1*UW.hat[[2]][,1]
  UW.hat[[2]][,1]<- -1*UW.hat[[2]][,1] ; UW.hat[[5]][,1]<- -1*UW.hat[[5]][,1] 
}

if(k0==1)
{
  UW.hat[[4]][,1]<- -1*UW.hat[[4]][,1] ; UW.hat[[5]][,1]<- -1*UW.hat[[5]][,1]
}


pdf("wtv_post.pdf",height=4,width=8,family="Times")

par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
#layout(matrix(c(1,2,1,3),2,2,byrow=TRUE));par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
###
cols<-c(2,3,4,8)
ru1<-range(sapply(UW.hat,function(x){ c(x[,1])}))
ru2<-range(sapply(UW.hat,function(x){ c(x[,2])})) ; ru<-range(c(0,ru1,ru2))
plot(ru*1.075,ru*1.075,type="n",xlab=expression(u[1]),ylab=expression(u[2]))
abline(h=0,col="gray",lty=2);abline(v=0,col="gray",lty=2)
for(j in 1:m){for(k in 1:K[j]){
 text(UW.hat[[j]][k,1],UW.hat[[j]][k,2],xlnames[[j]][k] ,col=cols[j])
 }}
for(j in 1:p) {
  text( UW.hat[[m+1]][j,1],UW.hat[[m+1]][j,2],xlnames[[m+1]][j] ) 
               }
###

mean( ( M.U(UW.hat)  - UW.pm )^2 )/mean( UW.pm^2) 


###
NB<-table( X[,1],X[,2],X[,3],X[,4] )
plot( c(NB,NB), c(Ybar-Bps/nss),xlab="sample size",
      ylab=expression(  bar(y)[x] - hat(mu[x])  ) )
abline(h=0,col="gray")

#plot( c(NB,NB), c(Bps/nss-EBps/nss),xlab="sample size",
#      ylab=expression(   hat(mu[x]) - hat(beta[x])  ) )
#abline(h=0,col="gray")
###


dev.off()

library(coda)
effectiveSize(VBPS[-(1:200),])
effectiveSize(VYPS[-(1:200),])
effectiveSize(B0[-(1:200),])

### age groups : -34, 35-47, 48-60, 61+
dim(X)

anova(lm(Y[,1]~(as.factor(X[,1])+as.factor(X[,2])+
                 as.factor(X[,3])+as.factor(X[,4]) )^2  ) )
anova(lm(Y[,1]~(as.factor(X[,1])+as.factor(X[,2])+
                 as.factor(X[,3])+as.factor(X[,4]) )^2  ) )

#interaction.plot(XY[,2]

