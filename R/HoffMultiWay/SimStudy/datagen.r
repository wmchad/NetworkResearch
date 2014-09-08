source("functions.r")

m<-3          # modes
K<-c(10,8,6)  # array dimensions

###  make a U0
U1<-list() ; R1<-8*6
for(i in 1:m)
  {
    V0<-rwish(diag(R1),R1+1)
    nu0<-R1+rpois(1,sqrt(R1))
    VU<-solve(rwish(V0,nu0))     # variance across ranks
    eU<-c(rmvnorm(1,rep(0,R1),VU))
    U1[[i]]<-rmn( rep(1,K[i])%*%t(eU),Srow=diag(K[i]),Scol=VU)
  }
U0<-U.als.psv(M.U(U1),R0,nsv=20)
###

### make a Y
M0<-M.U(U0)
M0<-M0/sqrt(mean(M0^2))
sig<-sqrt(mean(M0^2)/4)
Y<- M0 + array(rnorm(prod(K)),K)*sig
###
