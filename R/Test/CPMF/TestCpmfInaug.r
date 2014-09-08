require("reshape");
require("grid");
require("Matrix");
require("ggplot2");

## set.seed(12345)

setwd("c:/Code/NetworkResearch/R/Analysis/CPMF");
source("SymmetricMF.r");
setwd("c:/Code/NetworkResearch/R/Functions/Data/");
source("TowerFunctions.r");

CombineSnapshots <- function( year, month, day, hour, minute, nMin, nPeriods ) {
  X <- matrix(nrow=1139, ncol=1139, data=0);
  for ( p in 1:nPeriods ) {
    snap <- GetNetworkSnapshot2( year, month, day, hour, minute, nMin );
    snap <- snap[!is.na(snap$FromTower) & !is.na(snap$ToTower),];
    for ( i in 1:nrow(snap) ) {
      if ( snap$FromTower[i] > 0 & snap$FromTower[i] <= 1139 &
          snap$ToTower[i] > 0 & snap$ToTower[i] <= 1139 ) {
        X[snap$FromTower[i], snap$ToTower[i]] <- X[snap$FromTower[i], snap$ToTower[i]] +
          snap$Calls[i];
      }
    }
    minute <- minute+nMin;
    if ( minute >= 60 ) {
      minute <- minute - 60;
      hour <- hour + 1;
    }
  }
  X;
}

GetDetrendedSnapshot <- function( year, month, day, hour, minute ) {
  X <- matrix(nrow=1139, ncol=1139, data=0);
  curdir <- getwd();
  setwd(paste("c:/Data/NetworkResearch/NetworkSnapshots/20",
              Pad2(year), "-", Pad2(month), "/", Pad2(day), sep=""));
  detrendedFile <- paste("20", Pad2(year), "-", Pad2(month), "-", Pad2(day), "-DetrendedNetwork-",
                         Pad2(hour), Pad2(minute), ".txt", sep="");
  snap <- read.table(detrendedFile, header=TRUE);
  snap <- snap[snap$X1 <= snap$X2,];
  for ( i in 1:nrow(snap) ) {
    X[snap$X1[i], snap$X2[i]] <- X[snap$X2[i], snap$X1[i]] <- snap$value[i];
  }
  setwd(curdir);
  X;
}

BuildSymmetricSnapshotArray <- function( year, month, day, hour, minute,
                                nMin, nPeriods, nSnapshots, nTowers ) {
  Xarray <- array(data=0, dim=c(nTowers, nTowers, nSnapshots));
  for ( s in 1:nSnapshots ) {
    hr <- hour + floor((minute+(s-1)*nMin*nPeriods)/60);
    min <- (minute+(s-1)*nMin*nPeriods) %% 60;
    Xarray[,,s] <- CombineSnapshots( year, month, day, hr, min, nMin, nPeriods );
    Xarray[,,s] <- Xarray[,,s] + t(Xarray[,,s]);
    diag(Xarray[,,s]) <- diag(Xarray[,,s])/2;
  }
  Xarray;
}

DetrendedSnapshotArray <- function( year, month, day, hour, minute, nMin, nSnapshots, nTowers=1139 ) {
  Xarray <- array(data=0, dim=c(nTowers, nTowers, nSnapshots));
  for ( s in 1:nSnapshots ) {
    hr <- hour + floor((minute+(s-1)*nMin)/60);
    min <- (minute+(s-1)*nMin) %% 60;
    Xarray[,,s] <- GetDetrendedSnapshot( year, month, day, hr, min );
  }
  Xarray;
}


callData <- DetrendedSnapshotArray( 11, 7, 23, 8, 0, 30, 24, 1139 );
callData.adj <- callData - min(callData) + 1;
callData.adj[callData==0] <- 0;

ldim <- 5;
n <- 1139;
T <- 10;
lambda <- 0;
nobs <- sum(sapply( 1:T, function(t) {
  sum(callData[,,t] != 0) + sum(diag(callData[,,t]) !=0);
} ))/2;
beta <- n*ldim*log(nobs);
maxiter <- 20;
epsilonInter <- 0.01;
epsilonFinal <- 0.001;

betas <- c(100, 1000, 1e4, 1e5, 1e6, 1e7, 1e8);
ldims <- c(2,3,5,7,10,15);

cpResult.inaug <- NULL;
index <- 1;
for ( b in betas ) {
  for ( k in ldims ) {
    cpResult.inaug[[index]] <- list(beta=b, ldim=k,
                              result=SymmetricMF.Changepoint.PELT(
                                callData.adj, k, 0, b,
                                maxiter=maxiter,
                                epsilonInter=epsilonInter,
                                epsilonFinal=epsilonFinal,
                                verbose=FALSE));
    timestamp();
    print(paste("beta = ", b, ", ldim = ", k, " finished", sep=""));
    print(cpResult.inaug[[index]]$result$cp);
    print("");
    print("");
    index <- index + 1;
  }
}


calldataRes <- SymmetricMF.Changepoint.PELT( callData, ldim, lambda, beta, maxiter=maxiter,
                                             epsilonInter=epsilonInter,
                                             epsilonFinal=epsilonFinal,
                                             verbose=TRUE);

calldataRes.adj <- SymmetricMF.Changepoint.PELT( callData.adj, ldim, lambda, beta, maxiter=maxiter,
                                             epsilonInter=epsilonInter,
                                             epsilonFinal=epsilonFinal,
                                             verbose=TRUE);

calldataRes.adj2 <- SymmetricMF.Changepoint.PELT( callData.adj, ldim, lambda, beta, maxiter=maxiter,
                                             epsilonInter=epsilonInter,
                                             epsilonFinal=epsilonFinal,
                                             verbose=TRUE);

sapply(calldataRes$segments, function(r) {r$cost;});
##     1-2     3-3     4-4     5-5     6-6     7-8    9-10 
## 4165551 2195233 2178978 1910696 1538148 3185741 3753308 
## sum: 18927655







apply(callData, 3, sd);
## 1.850301 1.896952 1.907201 1.911820 1.801940 1.643249 1.601814 1.682976 1.735522 1.794715

sapply(1:T, function(t) {
  tData <- callData[,,t];
  sd(tData[tData !=0])
});
## 4.729136 4.874615 4.888203 4.963838 4.725398 4.364725 4.191171 4.373382 4.454433 4.619135

sapply(1:T, function(t) {
  tData <- callData[,,t];
  mu <- mean(tData);
  sum((tData[tData !=0]-mu)^2);
});
## 4415491 4641603 4692700 4717125 4194840 3491509 3314801 3655894 3887294 4155052
## sum: 41166307

sum(sapply(1:n, function(i) {
  sapply(1:n, function(j) {
    ijData <- callData[i,j,];
    mu <- mean(ijData);
    sum((ijData[ijData !=0]-mu)^2);
  });
}));
## 3848765









callData2 <- BuildSymmetricSnapshotArray( 11, 12, 6, 10, 0, 30, 1, 10, 1139 );

ldim <- 5;
lambda <- 100;
maxiter <- 10;
epsilon <- 1e-3;

symResult <- SymmetricMF( callData, ldim, lambda, NULL, maxiter, epsilon );
symResult2 <- SymmetricMF( callData2, ldim, lambda, NULL, 100, epsilon );

alsResult <- ALS( callData[,,1], ldim, lambda, NULL, NULL, maxiter, epsilon );

averageData <- matrix(nrow=1139, ncol=1139, data=0);
for ( t in 1:6 ) {
  averageData <- averageData + callData[,,t];
}
averageData <- averageData / 6;

alsAvgResult <- ALS( averageData, ldim, lambda, NULL, NULL, maxiter, epsilon );




symData <- BuildSymmetricSnapshotArray( 11, 12, 6, 11, 0, 30, 1, 1, 1139 );
symData <- symData[,,1];

symSums <- apply(symData, 1, sum);
goodRows <- which(symSums > 200);


towerLocs <- TowerLocations();

goodLocs <- towerLocs[goodRows,1:3];

eventLat <- towerLocs$Lat[790];
eventLong <- towerLocs$Long[790];

eventLocs <- TowersByDist(eventLat, eventLong, goodLocs);

eventRows <- eventLocs$TowerId;


setwd("c:/Data/NetworkResearch/Results/");
pdf( "DetrendedNetwork.pdf", height=10, width=10 );

for ( t in 1:10 ) {
  snap <- callData[,,t];
  snap <- snap[eventRows,];
  snap <- snap[,eventRows];

  snap.df <- melt(snap);
  snap.df$value <- sign(snap.df$value) * log(abs(snap.df$value))
  snap.df$value[is.nan(snap.df$value)] <- 0

  print(ggplot(snap.df, aes(X1, X2, fill=value)) + geom_raster() +
        scale_fill_gradient2(low="blue", mid="gray", high="red",
                             midpoint=0, guide=FALSE, limits=c(-6,6)));
}

dev.off();
