load("posterior.image")

library(coda)
s.scan<-seq(odens,S,by=odens)
vnames<-c("gdp sum","gdp product","polity product","jointly positive polity")

pdf("betacon.pdf",height=4,width=6,family="Times")
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
ylabs<-c("density","","density","")
for(j in 1:4)
{
  bj<-BETA[,j]
  bjk<-BETA[ -(1:(burn/odens)),j]
  dbj<-density(bjk,adj=2)
  hpdr<-hpd(dbj$x,dbj$y,.95)$hpdr
  plot(dbj,main="",xlab=vnames[j],ylab=ylabs[j])
  abline(v=0,col="gray",lty=2)
  segments(hpdr[1],0,hpdr[2],0,lwd=4,col="gray")

  cat(j,effectiveSize(bjk),"\n")
}

dev.off()
####



#####
set.seed(1)
source("functions.r")
UD.pm<-UDS.als(M.ps/nss,R,tol=1e-6)
U.pm<-UD.pm$U ; D.pm<-UD.pm$D
su<-sqrt(apply(U.pm^2,2,mean))
U.pm<-U.pm%*%diag(1/su)
D.pm<-D.pm%*%diag(su^2)
par(mfrow=c(3,3))
for(i in 1:dim(Y)[3])
{
  plot((M.ps/nss)[,,i],U.pm%*%diag(D.pm[i,])%*%t(U.pm))
  abline(0,1)
}

d2m<-apply(D^2,2,mean)
U.pm<-U.pm[, order(d2m,decreasing=TRUE) ]
D.pm<-D.pm[, order(d2m,decreasing=TRUE) ]

pdf("uudcon.pdf",height=4,width=8,family="Times")
layout(matrix(c(1,1,2,3),2,2))
par(mar=c(3,3,1,1),mgp=c(.75,.75,0))
mag<-apply(U.pm^2,1,sum)^(.15) ; mag<-mag/max(mag)
plot(U.pm[,1]*1.05,U.pm[,2]*1.05,type="n",
   xlab=expression(u[1]),ylab=expression(u[2]),xaxt="n",yaxt="n")
abline(h=0,col="gray") ; abline(v=0, col="gray")
text(U.pm[,1],U.pm[,2],dimnames(Y)[[1]],cex=mag)

par(mgp=c(1.75,.75,0))
labs<-c(expression(v[1]),expression(v[2]))
for(r in 1:2)
{
  plot(as.numeric(dimnames(Y)[[3]]),D.pm[,r],type="h",ylab=labs[r],xlab="",
       ylim=range(c(0,D.pm) ) ,lwd=3,col="gray" )
abline(h=0)
}
dev.off()

