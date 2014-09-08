### rank selection too hard - too much overlap
### possibly just see if criteria choses best model 
### in terms of MSE?

#### find selected model
R0<-4
seeds<-NULL
for(k in 1:10){ seeds<-c(seeds,1:10+(k-1)*10) }

PMAX<-NULL 
for(seed  in seeds )
{
  tmp<-dget(paste("LL.seed",seed,".R0",R0,sep=""))
  LL<-tmp$LL
  LLM<-tmp$LLMMM # max, mean, mode

  d1<- 2*(LLM[2,] - apply(LL,2,mean) ) # DIC mean
  d2<- 2*(LLM[3,] - apply(LL,2,mean) ) # DIC mode

  lp<- apply(LL,2,mean)  - .5*d1   # dic
  p<-exp(lp-max(lp))/sum( exp(lp-max(lp)) ) 
  PMAX<-c(PMAX,which.max(p))
}

print(table(PMAX) )
####


### 
MLL<-NULL
for( seed in seeds)
{
  tmp<-dget(paste("LL.seed",seed,".R0",R0,sep=""))
  cat(seed,tmp$LLMMM[1,],"\n")  
  MLL<-rbind(MLL,tmp$LLMMM[1,] )
}

MLL<- (MLL-apply(MLL,1,mean) )/apply(MLL,1,sd)

plot(c(1,5),range(MLL),type="n")
apply(MLL,1,lines)



