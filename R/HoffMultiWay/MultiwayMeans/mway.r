set.seed(1)

###
source("functions.r") ; library(abind) 
dat<-dget("wstv.dat") ;X<-dat$X; Y<-dat$Y
Y<-t( (t(Y)-apply(Y,2,mean))/apply(Y,2,sd))
m<-dim(X)[2]
N<-dim(Y)[1]
p<-dim(Y)[2]
X0<-matrix(0,N,dim(X)[2])
for(k in 1:m) { X0[,k]<-match(X[,k],sort(unique(X[,k]))) }
X<-X0
K<-apply(X,2,max)
R<-2
k0<-1e16
###

### 
manova(Y~as.factor(X[,1])+as.factor(X[,2])+as.factor(X[,3])+as.factor(X[,4]))


### group matching table ,  so uG[group.i[i],] == gI[i,]  is TRUE
gI<-X ; for(k in 1:m){ gI[,k]<-match( X[,k],sort(unique(X[,k])) ) }
gI<-as.matrix(gI)
uG<-matrix(1:K[1],K[1],1)
for(i in 2:m){ uG<-cbind( apply(uG,2,rep,times=K[i]) ,
               rep(1:K[i],rep(prod(K[1:(i-1)]),K[i]))) }
ucode<-runif(length(K))
uGcode<-uG%*%ucode
iGcode<-as.matrix(gI)%*%ucode
group.i<-match(iGcode,uGcode)
###

### starting values and priors
iVB0<-iVY0<-solve(cov(Y)) ; VY0<-VB0<-cov(Y)
eB0<-apply(Y,2,mean)/k0

B<-array(dim=c(K,0))
Xlist<-list() ; for(l in 1:m){Xlist[[l]]<-X[,l]} 
for(j in 1:p) { B<-abind(B,tapply(Y[,j],Xlist,mean)) }
Ybar<-B
B[is.na(B)]<-mean(B,na.rm=TRUE)
b<-apply(B,m+1,mean)

RB<- B - array(rep(b,rep(prod(K),p)),dim=c(K,p))
UW0<-U.als(RB,R)
U<-UW0[1:m] ; W<-UW0[[m+1]]

RRB<- RB - M.U( UW0)
VB<-cov( apply(RRB,m+1,c)) ; iVB<-solve(VB)

idx.yb<-NULL
for(i in 1:N) { for(j in 1:p) { idx.yb<-rbind(idx.yb,c(X[i,],j)) }}

EY<- matrix(B[idx.yb],N,p,byrow=TRUE)
RY<- Y-EY
VY<-cov(RY) ; iVY<-solve(VY)
###

MSE<-B0<-VBPS<-VYPS<-NULL  ; UWps<-Bps<-EBps<-B*0
S<-20000 ; burn<-2000 ; nss<-0 ; set.seed(1)
for(s in 1:(S+burn))  {

### update B given (b,U,W,VB,VY,Y)
EB<-array(rep(b,rep(prod(K),p)),dim=c(K,p)) + M.U(c(U,list(W)))
for(g in 1:dim(uG)[1])
{
  yg<-Y[group.i==g,,drop=FALSE ] 
  idx<-cbind( matrix(uG[g,],p,m,byrow=TRUE),1:p)
  eb<-EB[ idx ] 
  Vb<- solve( iVB + dim(yg)[1]*iVY )
  mb<- Vb%*%( iVB%*%eb + iVY%*%apply(yg,2,sum))
  B[idx]<-rmvnorm(1,mb,Vb)
}
###

### update  VY  given (B,Y)
EY<- matrix(B[idx.yb],N,p,byrow=TRUE)
RY<- Y-EY
SS<- VY0 + t(RY)%*%RY
iVY<-rwish(solve(SS),p+1+N) ; VY<-solve(iVY)
###

######## update U,W,VB given (B,b,VB,VY,Y)
tmp<-rUDpB.fc(B,U,W,eB0=eB0,VB0=VB0,k0=k0)
U<-tmp$U ; W<-tmp$D ; b<-tmp$eB ; iVB<-tmp$iVB ; VB<-solve(iVB)

########


if(s %%10==0) 
  { 
    EB<-array(rep(b,rep(prod(K),p)),dim=c(K,p)) + M.U(c(U,list(W)))
    if(s>burn)
    {  
      nss<-nss+1
      Bps<-Bps+B 
      EBps<-EBps+EB
      UWps<-UWps+ M.U(c(U,list(W)))
    }
    mse<-c(mean((B-Ybar)^2,na.rm=TRUE),mean((Bps/nss-Ybar)^2,na.rm=TRUE),
           mean((EB-Ybar)^2,na.rm=TRUE), mean((EBps/nss-Ybar)^2,na.rm=TRUE))
    MSE<- rbind(MSE,mse)
    B0<-rbind(B0,c(b,apply(B,m+1,mean)))
    VBPS<-rbind(VBPS, c(VB[upper.tri(VB,diag=TRUE)]))
    VYPS<-rbind(VYPS, c(VY[upper.tri(VY,diag=TRUE)]))

    cat(s,mse," ",round(apply(B,m+1,mean),2)," ",round(b,2),"\n")
    }
}

save.image("posterior.image")



