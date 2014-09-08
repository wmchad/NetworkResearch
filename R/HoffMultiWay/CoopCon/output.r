    BETA<-rbind(BETA,beta)
    sU<-sqrt(apply(U^2,2,sum)) ; Us<-t(t(U)/sU) ; Ds<-D%*%diag(sU^2)
    EU<-rbind(EU,apply(Us,2,mean))
    VU<-rbind(VU,c(var(Us)))
    ED<-rbind(ED,apply(Ds,2,mean) )
    VD<-rbind(VD,c(var(Ds)))

    ####
    ZS<- X.b(X,beta) + M.U(list(U,U,D))
    for(k in 1:K[3])
    {
      E<-matrix(rnorm(K[1]*K[1]),K[1],K[1])
      E[!upper.tri(E)]<-0
      E<-E+t(E)
      ZS[,,k]<-ZS[,,k]+E
    }
    YS<-ZS
    for(y in uy)
    {
      lb<- mean( c(max(Z[Y==y-1],-Inf ), min(Z[Y==y]) ) )
      ub<- mean( c(min(Z[Y==y+1],Inf), max(Z[Y==y])) )
      YS[ lb<ZS & ZS<ub]<- y
    }
    ####

    MU<-M.U(list(U,U,D))
    if(s>burn){
       nss<-nss+1
       M.ps<-M.ps+MU
       YPS<-YPS+YS
       UPS<-UPS+Us
       DPS<-DPS+Ds }
    out<-c( mean((M.ps/nss)^2),mean(MU^2), sum(MU^2)/sum( (Z-X.b(X,beta))^2),
            mean(abs( Y-YS)), mean(abs( Y-YPS/nss))   )
    OUT<-rbind(OUT,out)
    cat(s," ",round(beta,2)," ",mean((M.ps/nss)^2)," ",
        sum(MU^2)/sum( (Z-X.b(X,beta))^2)," ",
            mean(abs( Y-YS)),mean(abs( Y-YPS/nss)),"\n")
    if(s>burn & (s-burn)%%1000==0){
        save.image("posterior.image")
                                  }
#   par(mfrow=c(2,2))
#   for(j in 1:p) { plot(BETA[,j]) ;abline(h=0) }
   Up<-Us ; Dp<-Ds
   if(plt==TRUE)
    {
       par(mfcol=c(2,R),mar=c(3,2,1,1),mgp=c(1.75,.75,0))
       for(r in 1:R)
       {
      plot( sort(Up[,r]),type="n",xlab="",ylab="",xaxt="n",yaxt="n")
      abline(h=0)
      text(sort(Up[,r]),labels=(dimnames(Y)[[1]])[order(Up[,r])],
      srt=-45,cex=.8)
      plot(as.numeric(dimnames(Y)[[3]]),Dp[,r],type="h",ylab="",xlab="",
      ylim=range(c(0,Dp[,r]) )  )
      abline(h=0)
       }
    }

