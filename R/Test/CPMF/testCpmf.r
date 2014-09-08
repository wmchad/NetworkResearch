GenerateSimpleCPMF <- function( n, T, ldim, cps, sigmaX ) {
  Us <- lapply(c(1,cps), function(cp){
    matrix(data=1 + rnorm(n*ldim, 0, .5), nrow=n, ncol=ldim);
  });
  
  Xarray <- array(dim=c(n, n, T), data=rnorm(n*n*T, 0, sigmaX));
  tstarts <- c(1, cps+1);
  tends <- c(cps,T);
  for ( s in 1:length(tstarts) ) {
    X <- tcrossprod(Us[[s]]);
    for ( t in tstarts[s]:tends[s] ) {
      Xarray[,,t] <- Xarray[,,t] + X;
      for ( i in 1:(n-1) ) {
        for ( j in (i+1):n ) {
          Xarray[i,j,t] <- Xarray[j,i,t];
        }
      }
    }
  }
  list(X=Xarray, Us=Us, cps=cps, ldim=ldim, n=n, T=T, sigmaX=sigmaX);
}

setwd("c:/Code/NetworkResearch/R/Test/CPMF");
tests <- read.table("TestRuns.txt", header=TRUE, stringsAsFactors=FALSE);

for ( i in 1:nrow(tests) ) {
  testName <- tests$TestName[i];
  n <- tests$n[i];
  T <- tests$T[i];
  ldim <- tests$ldim[i];
  ldimcp <- tests$ldimcp[i];
  sigmaX <- tests$sigmaX[i];
  beta <- tests$beta[i];
  maxiter <- tests$maxiter[i];
  epsilonInter <- tests$epsilonInter[i];
  epsilonFinal <- tests$epsilonFinal[i];
  cps <- as.numeric(strsplit(tests$cps[i], ',')[[1]]);

  testData <- GenerateSimpleCPMF( n, T, ldim, cps, sigmaX );
  result <- SymmetricMF.Changepoint.PELT( testData$X, ldim, 0, beta,
                                          maxiter, epsilonInter,
                                          epsilonFinal, FALSE );
  cpmfTest <- list(data=testData, result=result);
  setwd("c:/Code/NetworkResearch/R/Test/CPMF/Results");
  save(cpmfTest, file=paste(testName, ".rdata", sep=""));
  print(paste(testName, "finished"));
}

for ( i in 1:nrow(tests) ) {
  testName <- tests$TestName[i];
  n <- tests$n[i];
  T <- tests$T[i];
  ldim <- tests$ldim[i];
  ldimcp <- tests$ldimcp[i];
  sigmaX <- tests$sigmaX[i];
  beta <- tests$beta[i];
  maxiter <- tests$maxiter[i];
  epsilonInter <- tests$epsilonInter[i];
  epsilonFinal <- tests$epsilonFinal[i];
  cps <- as.numeric(strsplit(tests$cps[i], ',')[[1]]);

  testData <- GenerateSimpleCPMF( n, T, ldim, cps, sigmaX );
  result <- SymmetricMF.Changepoint.OP( testData$X, ldim, beta,
                                        maxiter, epsilonInter,
                                        epsilonFinal, FALSE );
  cpmfTest.op <- list(data=testData, result=result);
  setwd("c:/Code/NetworkResearch/R/Test/CPMF/Results");
  save(cpmfTest.op, file=paste(testName, ".op.rdata", sep=""));
  print(paste(testName, "finished"));
}


cpmfResults.df <- data.frame( n=0, T=0, ldim=0, ldimcp=0, beta=0,
                              cps="", resultcps="",
                              found=0, missed=0, extra=0,
                              stringsAsFactors=FALSE );

for ( i in 1:nrow(tests) ) {
  testName <- tests$TestName[i];
  setwd("c:/Code/NetworkResearch/R/Test/CPMF/Results");
  load(paste(testName, ".rdata", sep=""));
  n <- tests$n[i];
  T <- tests$T[i];
  ldim <- tests$ldim[i];
  ldimcp <- tests$ldimcp[i];
  sigmaX <- tests$sigmaX[i];
  beta <- tests$beta[i];
  maxiter <- tests$maxiter[i];
  epsilonInter <- tests$epsilonInter[i];
  epsilonFinal <- tests$epsilonFinal[i];
  cps <- as.numeric(strsplit(tests$cps[i], ',')[[1]]);
  resultcps <- cpmfTest$result$cp
  found <- length(intersect(cps, resultcps));
  missed <- length(cps) - found;
  extra <- length(resultcps) - found;
  cpmfResults.df[i,] <- c(n, T, ldim, ldimcp, beta,
                          paste(cps, collapse=", "),
                          paste(resultcps, collapse=", "),
                          found, missed, extra);
}

tbl <- cpmfResults.df[,c("n", "T", "ldim", "ldimcp", "found", "missed", "extra")];

print(xtable(tbl[as.numeric(tbl$found) + as.numeric(tbl$missed) == 1,]),
       include.rownames=FALSE);

print(xtable(tbl[as.numeric(tbl$found) + as.numeric(tbl$missed) == 2,]),
       include.rownames=FALSE);

print(xtable(tbl[as.numeric(tbl$found) + as.numeric(tbl$missed) == 3,]),
       include.rownames=FALSE);

xtable(cpmfResults.df[,c("n", "T", "ldim", "ldimcp", "found", "missed", "extra")]);






n <- 20;
T <- 10;
ldim <- 5;
lambda <- 0;
cp <- 4;
sigmaU <- 1;
sigmaX <- 1;
beta <- n*ldim*log(T*(n^2+n)/2);
maxiter <- 20;
epsilonInter <- 0.01;
epsilonFinal <- 0.001;


testData <- GenerateSimpleCPMF( n, T, ldim, cp, sigmaX );

cpRes <- SymmetricMF.Changepoint.PELT( testData$X, ldim, lambda, beta, maxiter=maxiter,
                                  epsilonInter=epsilonInter,
                                  epsilonFinal=epsilonFinal);

cpRes.op <- SymmetricMF.Changepoint.OP( testData$X, ldim, beta, maxiter=maxiter,
                                  epsilonInter=epsilonInter,
                                  epsilonFinal=epsilonFinal);

n <- 100;
T <- 20;
ldim <- 5;
cp <- 10;
sigmaX <- 1;
beta <- n*ldim*log(T*(n^2+n)/2);
maxiter <- 20;
epsilonInter <- 0.01;
epsilonFinal <- 0.001;

testData2 <- GenerateSimpleCPMF( n, T, ldim, cp, sigmaX );

cpRes2 <- SymmetricMF.Changepoint.PELT( testData2$X, ldim, beta, maxiter=maxiter,
                                        epsilonInter=epsilonInter,
                                        epsilonFinal=epsilonFinal);

cpRes2.op <- SymmetricMF.Changepoint.OP( testData2$X, ldim, beta, maxiter=maxiter,
                                        epsilonInter=epsilonInter,
                                        epsilonFinal=epsilonFinal);
