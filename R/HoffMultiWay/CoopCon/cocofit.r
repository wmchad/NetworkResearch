####
dat<-dget("coco50-85.data")
set.seed(1)
####

####
Y<-dat$CC
K<-dim(Y)
X<-array(dim=c(K,5))
lgdp<-log(dat$GDP) ; lgdp<-lgdp-mean(lgdp)
for(k in 1:K[3]) 
{
  X[,,k,1]<-lgdp[,k]%*%t(rep(1,K[1])) + 
            t(lgdp[,k]%*%t(rep(1,K[1])))
  X[,,k,2]<-lgdp[,k]%*%t(rep(1,K[1])) *
            t(lgdp[,k]%*%t(rep(1,K[1])))
  X[,,k,3]<-1*(  (dat$PTY[,k]%*%t(dat$PTY[,k])) >0 )
  X[,,k,4]<- 1*( outer(dat$PTY[,k]>0,dat$PTY[,k]>0 ) )
  X[,,k,5]<-log(dat$DST) ; diag(X[,,k,5])<-0
}
X<-X[,,,c(1:5)[-c(5)]]   # take out distance
p<-dim(X)[4]

####
source("functions.r")
Z<- array(qnorm( rank(Y,ties.method="random")/(length(Y)+1) ),dim=K)
for(k in 1:K[3]) {Z[,,k]<- (Z[,,k]+t(Z[,,k]))/sqrt(2)}
XM<-NULL; for(j in 1:p){ XM<-cbind(XM,c(X[,,,j]))}
tmp<-lm( c(Z)~-1+ XM )
beta<-tmp$coef
E<-Z - X.b(X,beta)
tmp<-eigen(apply(E,c(1,2),mean))
R<-2
U<-tmp$vec[,1:R,drop=FALSE]
D<-rep(1,K[3])%*%t(tmp$val[1:R])
uy<-sort(unique(c(Y)))
UTA<-Z*NA
for(k in 1:K[3]) { UTA[,,k]<- upper.tri(Z[,,1] ) } 
UTA<-(UTA==1)
iVD<-iVU<-diag(R) ; eD<-eU<-rep(0,R) ; t20<-rep(1,R) ; eta0<-R+1
p<-dim(X)[4]
X0<-X ; for(k in 1:p) {  tmp<-X0[,,,k] ; tmp[!UTA]<-0 ; X0[,,,k]<-tmp }
XtX<-matrix(apply( apply(X0,c(1,2,3),function(x){x%*%t(x) } ),1,sum),p,p)
if(p==1) { XtX<-matrix(sum(X0^2),p,p) }
EU<-ED<-VU<-VD<-OUT<-BETA<-NULL
YPS<-Z*0
M.ps<-Y*0
burn<-5000
nss<-0
S<-500000+burn
odens<-10
UPS<-U*0 ; DPS<-D*0
plt<-FALSE
####


for(s in 1:S) {

  #### update Z
  EZ<- X.b(X,beta) + M.U(list(U,U,D))
  for(y in sample(uy))
  { 
    lb<-suppressWarnings(max(Z[Y<y & UTA])) 
    ub<-suppressWarnings(min(Z[Y>y & UTA]))
    z<-qnorm(runif(sum(Y==y),pnorm( lb-EZ[Y==y] ), pnorm(ub-EZ[Y==y])))
    Z[Y==y]<- EZ[Y==y]+ z
  }

  for(k in 1:K[3]) 
  { 
     tmp<-Z[,,k]  ; tmp[!UTA[,,k]]<-0
     Z[,,k]<-tmp+t(tmp)  ; diag(Z[,,k])<- 0
  }
  ####

  #### update U
  ZE<- Z- X.b(X,beta)
  for(i in sample(K[1]))
  {
    Ui<-U ; Ui[i,]<-0
    DU<- aperm(array(apply(Ui,1,"*",t(D)),dim=c(R,K[3],K[1])),c(3,2,1))
    zi<-ZE[i,,]
    L<- apply(DU*array(rep(zi,R),dim=c(K[1],K[3],R)),3,sum) 
    Q<- (t(Ui)%*%Ui ) * ( t(D)%*%D )
    cV<-solve( Q + iVU )
    cE<-cV%*%( L + iVU%*%eU) 
    U[i,]<-rmvnorm( 1, cE, cV ) 
  } 
  ####

  #### update D
  ZE[!UTA]<-0
  Q<-((t(U)%*%U)^2-
       matrix(apply(apply(U^2,1,function(x){x%*%t(x)}),1,sum),R,R))/2
  UU<-aperm(array( apply(U,1,"*",t(U)) ,dim=c(R,K[1],K[1]) ),c(2,3,1))
  ZEP<-aperm(ZE,c(3,1,2))
  ZUU<-array(apply(UU,3,function(x){apply(ZEP,1,"*",x)}),
       dim=c(K[1],K[1],K[3],R))
  L<-apply(ZUU,c(3,4),sum)

  cV<-solve( Q + iVD)
  cE<-(L + rep(1,K[3])%*%t(eD)%*%iVD)%*%cV
  D<-rmn( cE,diag(K[3]),cV)
  ####

  #### update beta
  ZE<-Z- M.U(list(U,U,D))
  Xtz<- t(apply(X0,4,c))%*%c(ZE)
  cV<-solve( XtX + diag(1/100,p))
  cE<-cV%*%Xtz
  beta<-rmvnorm(1,cE,cV)
  ####

  #### update hierarchical parameters
  msi<-outer(apply(U,2,mean),apply(U,2,mean) )
  SS<- diag(t20,nrow=R) +  (K[1]-1)*cov(U)   +K[1]*msi/(K[1]+1)
  iVU<-rwish(solve(SS), eta0+K[1])
  eU<-c(rmvnorm(1,apply(U,2,sum)/(K[1]+1),solve(iVU)/(K[1]+1)))

  msi<-outer(apply(D,2,mean),apply(D,2,mean) )
  SS<- diag(t20,nrow=R) +  (K[3]-1)*cov(D)   +K[3]*msi/(K[3]+1)
  iVD<-rwish(solve(SS), eta0+K[3])
  eD<-c(rmvnorm(1,apply(D,2,sum)/(K[3]+1),solve(iVD)/(K[3]+1)))
  ####

  #### output 
  if(s%%odens==0 )  { source("output.r") }
  ####

}
