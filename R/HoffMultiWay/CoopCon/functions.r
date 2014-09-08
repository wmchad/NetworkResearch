##### One iteration of the alternating least-squares algorithm
U.als.step<-function(X,U0)
{
  U<-U0
  m<-length(U)
  for(l in  sample(1:m))
  {
    lq.l<-LQ(X,U,l)
    U[[l]]<-lq.l$L%*%solve(lq.l$Q)
  }

  U
}
#####


#####
U.als<-function(X,R,tol=.01)
{
  K<-dim(X) ; m<-length(K)
  U0<-list() ; for(i in 1:m){ U0[[i]]<-rsmn(K[i],R) }
  rel.diff<- tol+1
  while(rel.diff>tol)
  {
    U<-U.als.step(X,U0)
    rel.diff<- mean(  (M.U(U)-M.U(U0))^2 )/mean( M.U(U0)^2)
    U0<-U
  }
  U
}
#####

###
X.b<-function(X,b)
{  
  p<-dim(X)[length(dim(X)) ]
  tmp<-array(0,dim(X)[-length(dim(X))])
  for(j in 1:p) {tmp<-tmp+X[,,,j]*b[j] } 
  tmp
}
###


###
rsmn<-function (m, n)
{
  matrix(rnorm(m * n), m, n)
}
##


##
#####
library(abind)
#####

##### finds linear and quadratic parts of lik for X ~ normal( M.U(U) , I )
LQ<-function(X,U,l)
{

  r<-dim(U[[1]])[2]
  m<-length(U)
  n<-sapply(U,length)/r

  Q<-matrix(apply(
     matrix(sapply(U[-l],function(x){t(x)%*%x}), r*r,m-1),1,prod),r,r)

  U2Um<-array(dim=c(n[-l],0))
  for(k in 1:r)
  {
    tmp<-U[[ ((1:m)[-l])[1] ]][,k]
    for(j in ((1:m)[-l])[-1] )
    {
      tmp<-outer(tmp,U[[j]][,k])
    }
    U2Um<-abind(U2Um,tmp)
  }

  XU2Um<-array(apply(U2Um,m,function(x){apply(X,l,"*",x)}),dim=c(n[-l],n[l],r))
  L<-apply(XU2Um,c(m,m+1),sum)

 list(L=L,Q=Q)
}
#####



##### Create array out of factors
M.U<-function(U)
{
  r<-dim(U[[1]])[2]
  m<-length(U)
  n<-sapply(U,length)/r

  M<-array(0,dim=n)

  for(k in 1:r)
  {
    tmp<-U[[1]][,k]
    for(j in (2:m))
    {
      tmp<-outer(tmp,U[[j]][,k])
    }
    M<-M+tmp
  }
  M
}
#####



#####



##
rmvnorm<-function(n,mu,Sigma)
{
  E<-matrix(rnorm(n*length(mu)),n,length(mu))
  t(  t(E%*%chol(Sigma)) +c(mu))
}
##

##
rsmn<-function (m, n)
{
  matrix(rnorm(m * n), m, n)
}
##

##
rmn<-function (M = 0, Srow, Scol)
{
  m <- dim(Srow)[1]
  n <- dim(Scol)[1]
  tmp <- eigen(Srow)
  Srow.h <- tmp$vec %*% diag(sqrt(tmp$val),nrow=m) %*% t(tmp$vec)
  tmp <- eigen(Scol)
  Scol.h <- tmp$vec %*% diag(sqrt(tmp$val),nrow=n) %*% t(tmp$vec)
  Z <- rsmn(m, n)
  Srow.h %*% Z %*% Scol.h + M
}
##

##
rwish<-function(S0,nu)
{
  # sample from a Wishart distribution
  sS0<-chol(S0)
  Z<-matrix(rnorm(nu*dim(S0)[1]),nu,dim(S0)[1])%*%sS0
  t(Z)%*%Z
  # expectation is S0*nu
  # S0 appears as S0^{-1} in the exponent of the Wishart density
}
##

## 

UDS.als<-function(Y,R,tol=1e-5)
{
  m<-dim(Y)[1] ; n<-dim(Y)[3]
  U<-matrix(rnorm(m*R),m,R) ; D<-matrix(rnorm(n*R),n,R)
  M0<-M.U(list(U,U,D))
  rdif<-1  

  Y0<-Y
  for(k in 1:n) { Y0[,,k][!upper.tri(Y0[,,k])]<- 0 }
  Y0<-aperm(Y0,c(3,1,2))


  while(rdif>tol) 
  {

    for(i in sample(m))
    {
      Ui<-U ; Ui[i,]<-0
      DU<- aperm(array(apply(Ui,1,"*",t(D)),dim=c(R,n,m)),c(3,2,1))
      zi<-Y[i,,]
      L<- apply(DU*array(rep(zi,R),dim=c(m,n,R)),3,sum)
      Q<- (t(Ui)%*%Ui ) * ( t(D)%*%D )
      U[i,]<-solve(Q)%*%L
    }
  
    Q<-((t(U)%*%U)^2-
        matrix(apply(apply(U^2,1,function(x){x%*%t(x)}),1,sum),R,R))/2
 
    UU<-aperm(array( apply(U,1,"*",t(U)) ,dim=c(R,m,m) ),c(2,3,1))
    ZUU<-array(apply(UU,3,function(x){apply(Y0,1,"*",x)}),
         dim=c(m,m,n,R))
    L<-apply(ZUU,c(3,4),sum)
    D<-L%*%solve(Q)
  
    M1<-M.U(list(U,U,D))  
    rdif<- mean( (M1-M0)^2 )/mean(M0^2) 
    M0<-M1
  }
  list(U=U,D=D)
}







