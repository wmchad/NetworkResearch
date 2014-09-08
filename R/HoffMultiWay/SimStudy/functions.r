####
lpy.z<-function(z,K=30)
{
  ez<-mean(z)
  k<-0:K
  coef<- (-1)^k/gamma(k+1)
  moments<-apply( outer(z-ez,k,"^"),2,mean )
  ez - log(cumsum(coef*moments)   )
}

mlpz<-function(theta,z)
{
  ez<-theta[1] ; sz<-exp(theta[2]) ; a<-exp(theta[3]); b<-exp(theta[4])
  lpz<-dbeta( pnorm( (z-ez)/sz),a,b,log=TRUE) +
       dnorm(z,ez,sz,log=TRUE)
  -sum(lpz)
}

integrand<-function(z,theta)
{
  ez<-theta[1] ; sz<-exp(theta[2]) ; a<-exp(theta[3]); b<-exp(theta[4])
  lpz<-dbeta( pnorm( (z-ez)/sz),a,b,log=TRUE) +
       dnorm(z,ez,sz,log=TRUE)
  exp(lpz-z)
}


lpy.z<-function(z)
{
  fit<-optim(c(mean(z),log(sd(z)),0,0),mlpz,z=z)
  integrate( integrand,lower=-Inf,upper=Inf,theta=fit$par)$val + mean(ll)
}

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
U.als<-function(X,R,tol=.01,U0=NULL)
{
  K<-dim(X) ; m<-length(K)
  if(is.null(U0)) {U0<-list() ; for(i in 1:m){ U0[[i]]<-rsmn(K[i],R) } }
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



##### Sample from the Wishart, multivariate and matrix normal distributions

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
rwish<-function(S0,nu)
{
  # sample from a Wishart distribution
  sS0<-try(chol(S0),silent=TRUE)
  if(is.character(sS0)){r<-eigen(S0);sS0<-r$vec%*%diag(sqrt(r$val))%*%t(r$vec)}
  Z<-matrix(rnorm(nu*dim(S0)[1]),nu,dim(S0)[1])%*%sS0
  t(Z)%*%Z
  # expectation is S0*nu
  # S0 appears as S0^{-1} in the exponent of the Wishart density 
}
##





#####


##
U.als.msv<-function(X,R,tol=1e-6,nsv=10)
{
  
  UL<-list()
  err<-NULL
  for(s in 1:nsv)
  {
    UL[[s]]<-U.als(X,R,tol=tol) 
    err<-c(err,mean( (X-M.U(UL[[s]]))^2 ) )
  }

  UL[[which.min(err)]]
  
}


###
U.als.psv<-function(X,R,U0=NULL,tol=1e-6,nsv=10)
{

  UL<-list()
  err<-NULL
  m<-dim(X)
  for(s in 1:nsv)
  { 
    if(!is.null(U0))
    {
      rm<- R-dim(U0[[1]])[2]
      for(i in 1:length(m))
      { 
        Ui<-U0[[i]]
        scl<-sqrt(m[i])*rgamma(1,10,10)
        si<-apply(Ui,2,sd)/scl ; si<-c(si,rep(min(si),rm))
        Ui<-cbind(Ui,matrix(0,nrow=m[i],ncol=rm))
        U0[[i]]<-Ui+matrix(rnorm(R*m[i]),m[i],R)%*%diag(si) 
      } 
    }
    UL[[s]]<-U.als(X,R,tol=tol,U0)
    err<-c(err,mean( (X-M.U(UL[[s]]))^2 ) )
  }

  UL[[which.min(err)]]
 
}
###



