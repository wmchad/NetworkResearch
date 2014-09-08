library(coda)

S<-11000 ; burn<-1000 ; odens<-1

R0<-4
KS<-10
seeds<- 1:10 + (KS-1)*10
MS<-NULL

for(seed in seeds)
{
  set.seed(seed)
  source("datagen.r")  # generate data
  for(R in 1:8)     # for each presumed rank
  {
    source("hbayes.r") 
    mse<-c(mean(Y^2),                 # size of matrix
           mean((Y-M0)^2),            # MSE of Y
           mean((Y-M.ps/nss)^2) ,     # RSS of mean
           mean((M0-M.ps/nss)^2),     # MSE of of mean 
           mean((Y-M.mode)^2) ,       # RSS of mode
           mean((M0-M.mode)^2),       # MSE of mode
	   mean((Y-M.ols)^2),         # RSS of ols
           mean((M0-M.ols)^2))        # MSE of ols
    cat(seed,R,round(100*mse,3)," ",round(DIC,2),"\n")
    MS<-rbind(MS,c(seed,R,mse,DIC,eSS))
  }
  dput(MS,paste("hbayes.SS",KS,".R0",R0,".results",sep=""))
}


