###
HRES<-NULL
for(k in (1:10)){HRES<-rbind(HRES,
  dget(paste("hbayes.SS",k,".R04.results",sep=""))) }
###


##########  R0=4, R=4 simulation
median(HRES[HRES[,2]==4,12] )
# column 8 is mode,  6 is mean 
hres.mode<-HRES[HRES[,2]==4,8]/HRES[HRES[,2]==4,4]
hres.mean<-HRES[HRES[,2]==4,6]/HRES[HRES[,2]==4,4]
ares<-HRES[HRES[,2]==4,10]/HRES[HRES[,2]==4,4]
rng<-range(c(hres.mode,hres.mean,ares))

pdf("mserss4.pdf",height=4,width=8,family="Times")
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(ares,hres.mode,pch=16,ylim=rng,xlim=rng,
      xlab="least squares MSE",ylab="Bayesian MSE")
points(ares,hres.mean,pch=16,col="gray")
abline(0,1)
legend(.075,.4,legend=c("mode","mean"),
       pch=c(16,16),col=c("black","gray"),bty="n")

1-range(ares)
1-range(hres.mode/ares)
1-mean(hres.mode/ares)

### make a similar plot with RSS
### to show its not als deficiency
hres.mode<-HRES[ HRES[,2]==4,7]/HRES[HRES[,2]==4,3]
hres.mean<-HRES[HRES[,2]==4,5]/HRES[HRES[,2]==4,3]
ares<-HRES[HRES[,2]==4,9]/HRES[HRES[,2]==4,3]

rng<-range(c(hres.mode,hres.mean,ares))

plot(ares,hres.mode,pch=16,ylim=rng,xlim=rng,
      xlab="least squares RSS",ylab="Bayesian RSS")
points(ares,hres.mean,pch=16,col="gray")
abline(0,1)
legend(.075,.4,legend=c("mode","mean"),
       pch=c(16,16),col=c("black","gray"),bty="n")
dev.off()

IRES<-NULL
for(k in (1:10)){IRES<-rbind(IRES,
  dget(paste("ibayes.SS",k,".R04.results",sep=""))) }

hres.mode<-HRES[HRES[,2]==4,8]/HRES[HRES[,2]==4,4]
hres.mean<-HRES[HRES[,2]==4,6]/HRES[HRES[,2]==4,4]
ires.mode<-IRES[IRES[,2]==4,8]/IRES[IRES[,2]==4,4]
ires.mean<-IRES[IRES[,2]==4,6]/IRES[IRES[,2]==4,4]

mean(ires.mean>hres.mean)
mean(ires.mode>hres.mode)
##########



##########  misspecified rank 
pdf("mserss18.pdf",height=4.5,width=7,family="Times")
head=c("least squares","hierarchical Bayes")
ylab2<-c("log MSE","")
ylab1<-c("log RSS","")

par(mfcol=c(2,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
for(i in 1:2)
{
  R<-HRES[,2]
  mse.m<-HRES[,10-(i-1)*4]/HRES[,4]
  mse.y<-HRES[,9-(i-1)*4]/HRES[,3]
  boxplot(log(mse.y)~R,col="gray",ylim=c(-2.75,-.25),xlab="",ylab=ylab1[i])
  mtext(head[i],side=3,cex=.8)
  boxplot(log(mse.m)~R,col="gray",ylim=c(-3,.75),xlab="assumed rank",
          ylab=ylab2[i])
}
dev.off()
##########



########## rank selection

HRES2<-NULL
for(k in (1:10)){HRES2<-rbind(HRES2,dget(
   paste("hbayes.SS",k,".R02.results",sep=""))) }

HRES6<-NULL
for(k in (1:10)){HRES6<-rbind(HRES6,dget(
   paste("hbayes.SS",k,".R06.results",sep=""))) }

HRESR<-HRES6
MBP<-NULL
for(seed in 1:100)
{
  tmp<-HRESR[HRESR[,1]==seed,]
  bpick<-which.min(tmp[,11])
  mpick<-which.min(tmp[,8])
  MBP<-rbind(MBP,c(bpick,mpick))
}

table(MBP[,1])/100
table(MBP[,1],MBP[,2])

mean(MBP[,1]<6) 

