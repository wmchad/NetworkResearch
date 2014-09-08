## given R, Y

## setup constants
K<-dim(Y) ; m<-length(K)
source("functions.r")
##

## starting values and unit info priors
set.seed(1)

###
U<-U.als.msv(Y,R,nsv=20)
M.ols<-M.U(U) ; s2.ols<-mean( (Y-M.ols)^2 )
llmx<- sum(log(dnorm(c(Y),M.ols,sqrt(s2.ols))) )
###

## priors
nu0<- 1 ;  s20<-var( c(Y-M.U(U)) ) 
iVU<-array(diag(1/100,R),dim=c(R,R,m))
eU<-rep(0,R)
##

## start values
s2<-s2.ols
M.ps<-M.U(U)*0
nss<-0
##

## MCMC
M2<-ll<-S2<-NULL
for(s in 1:S) 
{

  ## update U
  for(i in sample(1:m))
  {
     #msi<-outer(apply(U[[i]],2,mean),apply(U[[i]],2,mean) )
     #SS<- diag(t20,nrow=R) +  (K[i]-1)*cov(U[[i]])   +K[i]*msi/(K[i]+1)
     #iVU[,,i]<-rwish(solve(SS), eta0+K[i])
     #eU<-c(rmvnorm(1,apply(U[[i]],2,sum)/(K[i]+1),solve(iVU[,,i])/(K[i]+1))) 
 
     lq<-LQ(Y,U,i)  
     V<-solve(lq$Q/s2+iVU[,,i])
     M<-(lq$L/s2 + rep(1,K[i])%*%t(eU)%*%iVU[,,i])%*%V
     U[[i]]<-rmn(M,diag(1,nrow=K[i]),V)
  }
  ##  
 
  ## update s2
  s2<-1/rgamma(1, (nu0+prod(K))/2, (nu0*s20+ sum( (Y-M.U(U))^2))/2  )
  ##

  ## output
  if(s%%odens==0 & s>burn)
  {
    nss<-nss+1
    S2<-c(S2,s2)
    M2<-c(M2,mean(M.U(U)^2))
    M.ps<-M.ps+M.U(U)
    MU<-NULL ; for(i in 1:m) {MU<-rbind(MU,U[[i]]) }
    ll<-c(ll, sum(log(dnorm(c(Y),c(M.U(U)),sqrt(s2))) ) )
    ### 
  } 
  ##

}

eSS<-effectiveSize(M2)


### posterior estimates and  DIC 
M.mean<-M.ps/nss
s2.mean<-mean( (Y-M.mean)^2 )
DIC<-2*mean(-2*ll)-(-2*sum(log( dnorm(c(Y),c(M.mean),sqrt(s2.mean)))))

M.mode<-M.U( U.als.msv(M.ps/nss,R=R))
s2.mode<-mean( (Y-M.mode)^2 )
###

