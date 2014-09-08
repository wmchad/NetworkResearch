require("reshape");
require("grid");
require("ggplot2");
require("Matrix");

## set.seed(12345)

setwd("c:/Code/NetworkResearch/R/Analysis/TimeALS");
source("TimeALS.r");
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

BuildSnapshotArray <- function( year, month, day, hour, minute,
                                nMin, nPeriods, nSnapshots, nTowers ) {
  Xarray <- array(data=0, dim=c(nTowers, nTowers, nSnapshots));
  for ( s in 1:nSnapshots ) {
    hr <- hour + floor((minute+(s-1)*nMin*nPeriods)/60);
    min <- (minute+(s-1)*nMin*nPeriods) %% 60;
    Xarray[,,s] <- CombineSnapshots( year, month, day, hr, min, nMin, nPeriods );
  }
  Xarray;
}

Ldf.adj <- function( L, towers, adjTower=NULL ) {
  if ( !is.null(adjTower) ) {
    adj <- L[towers==adjTower,];
    L <- t(apply(L, 1, function(l){l - adj;}));
  }
  df <- data.frame( towerId=towers );
  for ( i in 1:ncol(L) ) {
    df[,paste("L", i, sep="")] <- L[,i];
  }
  df;
}

RotateDf <- function(df, refTower, l1="L1", l2="L2") {
  x <- df[,l1];
  y <- df[,l2];
  c <- complex(real=x, imaginary=y);
  r <- Mod(c);
  theta <- Arg(c);
  theta <- theta - theta[df$towerId==refTower];
  df[,l1] <- r * cos(theta);
  df[,l2] <- r * sin(theta);
  df;
}

Ldf.adj.rot <- function(L, towers, adjTower, refTower, l1="L1", l2="L2") {
  RotateDf(Ldf.adj(L, towers, adjTower), refTower, l1, l2);
}

SimXarray <- function( L1, R1, n1, sd1, L2, R2, n2, sd2, sdx ) {
  Larray <- array(data=0, dim=c(nrow(L1), ncol(L1), n1+n2));
  Rarray <- array(data=0, dim=c(nrow(R1), ncol(R1), n1+n2));
  Xarray <- array(data=0, dim=c(nrow(R1), nrow(R1), n1+n2));
  nNorm <- nrow(L1) * ncol(L1);
  Larray[,,1] <- L1;
  for ( i in 2:n1 ) {
    Larray[,,i] <- Larray[,,i-1] + rnorm(nNorm, 0, sd1);
  }
  Larray[,,n1+1] <- L2;
  for ( i in (n1+2):(n1+n2) ) {
    Larray[,,i] <- Larray[,,i-1] + rnorm(nNorm, 0, sd1);
  }
  Rarray[,,1] <- R1;
  for ( i in 2:n1 ) {
    Rarray[,,i] <- Rarray[,,i-1] + rnorm(nNorm, 0, sd2);
  }
  Rarray[,,n1+1] <- R2;
  for ( i in (n1+2):(n1+n2) ) {
    Rarray[,,i] <- Rarray[,,i-1] + rnorm(nNorm, 0, sd1);
  }
  nX <- nrow(L1)^2;
  for ( i in 1:(n1+n2) ) {
    Xarray[,,i] <- Larray[,,i] %*% t(Rarray[,,i]) + rnorm(nX, 0, sdx);
  }
  list(L=Larray, R=Rarray, X=Xarray);
}

ldim <- 5;
lambda <- 10;
niter <- 1000;
epsilon <- 1e-6;

X1 <- BuildSnapshotArray( 11, 12, 6, 11, 30, 5, 1, 1, 1139 )[,,1];
X2 <- BuildSnapshotArray( 11, 12, 6, 11, 55, 5, 1, 1, 1139 )[,,1];

als1 <- ALS(X1, ldim, 0.001, NULL, NULL, 100, 1e-06);
als2 <- ALS(X2, ldim, 0.001, NULL, NULL, 100, 1e-06);

rows <- c(sample(1139, 98), 405, 790);

simData <- SimXarray(als1$L[rows,], als1$R[rows,], 20, .1,
                     als2$L[rows,], als2$R[rows,], 20, .1, 1);


simResult <- Tmf.ALS(simData$X, ldim, lambda, NULL, NULL, niter, epsilon);

dists <- matrix(data=0, nrow=dim(simResult$L)[1], ncol=dim(simResult$L)[3]-1);
for ( i in 2:dim(simResult$L)[3] ) {
  for ( j in 1:dim(simResult$L)[1] ) {
    dists[j,i-1] <- sqrt(sum((simResult$L[j,,i]-simResult$L[j,,i-1])^2));
  }
}

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(dists[i,], type="l");}
par(mfrow=c(1,1));

interestingIndices <- c(2,3,13,17);

setwd("c:/Plots/NetworkResearch/Temp/");
pdf( "ExampleLatentDistances.pdf", height=10, width=10 );
par(mfrow=c(2,2));
for ( i in interestingIndices ) {plot(dists[i,], type="l");}
dev.off();
par(mfrow=c(1,1));

fulldists <- sapply(2:dim(simResult$L)[3], function(i) {
  sqrt(sum((simResult$L[,,i]-simResult$L[,,i-1])^2));
});

pdf( "CombinedLatentDistances.pdf", height=10, width=10 );
plot(fulldists, type="l");
dev.off();

truedists <- matrix(data=0, nrow=dim(simData$L)[1], ncol=dim(simData$L)[3]-1);
for ( i in 2:dim(simData$L)[3] ) {
  for ( j in 1:dim(simData$L)[1] ) {
    truedists[j,i-1] <- sqrt(sum((simData$L[j,,i]-simData$L[j,,i-1])^2));
  }
}

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(truedists[i,], type="l");}
par(mfrow=c(1,1));

truefulldists <- sapply(2:dim(simData$L)[3], function(i) {
  sqrt(sum((simData$L[,,i]-simData$L[,,i-1])^2));
});

plot(truefulldists, type="l");


timeDists <- sapply(1:dim(simResult$L)[3], function(t) {
  UV <- simData$L[,,t] %*% t(simData$R[,,t]);
  UVhat <- simResult$L[,,t] %*% t(simResult$R[,,t]);
  diff <- UV - UVhat;
  sqrt(sum(diag(diff %*% t(diff))));
});

frobhat <- sapply(1:dim(simResult$L)[3], function(t) {
  UVhat <- simResult$L[,,t] %*% t(simResult$R[,,t]);
  sqrt(sum(diag(UVhat %*% t(UVhat))));
});

trueFrob <- sapply(1:dim(simResult$L)[3], function(t) {
  UV <- simData$L[,,t] %*% t(simData$R[,,t]);
  sqrt(sum(diag(UV %*% t(UV))));
});

yrng <- range(c(timeDists, frobhat, trueFrob));

setwd("c:/Plots/NetworkResearch/Temp/");

pdf("FrobDiff.pdf", height=8, width=8);
plot(timeDists, type="l", xlab="Timepoint", ylab="Frob(UV' - UhatVhat')");
dev.off();

pdf("FrobComparison.pdf", height=8, width=8);
plot(trueFrob, type="l", col="blue", xlab="Timepoint", ylab="Frob(UV')");
lines(frobhat, col="red");
legend( 30, 1500, legend=c("Truth", "Inferred"), col=c("blue", "red"), lty=1 );
dev.off();

setwd("c:/Data/NetworkResearch/Results/");
save(simData, file="simData.rdata");
save(simResult, file="simResult.rdata");





bombNetData <- BuildSnapshotArray( 11, 12, 6, 10, 0, 5, 1, 48, 1139 );

ldim <- 5;
lambda <- 10
niter <- 1000;
epsilon <- 1e-6;

bombTMF <- Tmf.ALS(bombNetData, ldim, lambda, NULL, NULL, niter, epsilon);

setwd("c:/Data/NetworkResearch/Results/");
save(bombTMF, file="bombTMF.rdata");

bombFrob <- sapply(1:dim(bombTMF$L)[3], function(t) {
  UVhat <- bombTMF$L[,,t] %*% t(bombTMF$R[,,t]);
  sqrt(sum(diag(UVhat %*% t(UVhat))));
});

latentDists.send <- matrix(data=0, nrow=dim(bombTMF$L)[1], ncol=dim(bombTMF$L)[3]-1);
for ( i in 2:dim(bombTMF$L)[3] ) {
  for ( j in 1:dim(bombTMF$L)[1] ) {
    latentDists.send[j,i-1] <- sqrt(sum((bombTMF$L[j,,i]-bombTMF$L[j,,i-1])^2));
  }
}

latentDists.rec <- matrix(data=0, nrow=dim(bombTMF$R)[1], ncol=dim(bombTMF$R)[3]-1);
for ( i in 2:dim(bombTMF$L)[3] ) {
  for ( j in 1:dim(bombTMF$L)[1] ) {
    latentDists.rec[j,i-1] <- sqrt(sum((bombTMF$R[j,,i]-bombTMF$R[j,,i-1])^2));
  }
}

par(mfrow=c(5,5));
for ( i in sample(1139, 25) ) {plot(latentDists.send[i,], type="l");}
par(mfrow=c(1,1));

plot(latentDists.send[405,], type="l");
plot(latentDists.send[790,], type="l");

fullLatentDists.send <- sapply(2:dim(bombTMF$L)[3], function(i) {
  sqrt(sum((bombTMF$L[,,i]-bombTMF$L[,,i-1])^2));
});

fullLatentDists.rec <- sapply(2:dim(bombTMF$R)[3], function(i) {
  sqrt(sum((bombTMF$R[,,i]-bombTMF$R[,,i-1])^2));
});

plot(fullLatentDists.send, type="l")
plot(fullLatentDists.rec, type="l")

fullDists <- data.frame(time=1:47, send=fullLatentDists.send,
                        receive=fullLatentDists.rec);



towerLocs <- TowerLocations();

eventLat <- 34.51828384;
eventLong <- 69.18315125;

eventTowers <- TowersByDist(eventLat, eventLong, towerLocs);


par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.send[eventTowers$TowerId[i],], type="l", ylim=c(0, 2));}
par(mfrow=c(1,1));

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.rec[eventTowers$TowerId[i],], type="l", ylim=c(0, 2));}
par(mfrow=c(1,1));

towerCalls <- apply(bombNetData, 1, sum);

highTrafficTowers <- (1:1139)[towerCalls >= 1000];
evTowers2 <- eventTowers[order(eventTowers$TowerId),];
trafficTowers <- evTowers2[highTrafficTowers,];
trafficTowers <- trafficTowers[order(trafficTowers$Dist),];


subDists.send <- latentDists.send[trafficTowers$TowerId,];
subDists.rec <- latentDists.rec[trafficTowers$TowerId,];

df.send <- melt(subDists.send);
colnames(df.send) <- c("Tower", "Timepoint", "DistMove");
df.send$Dist <- trafficTowers$Dist[df.send$Tower];

df.rec <- melt(subDists.rec);
colnames(df.rec) <- c("Tower", "Timepoint", "DistMove");
df.rec$Dist <- trafficTowers$Dist[df.rec$Tower];

ggplot(data=df.send, aes(Timepoint, Tower, fill=DistMove)) + geom_raster();

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.send[trafficTowers$TowerId[i],],
                        type="l", ylim=c(0, 2),
                        ylab="", main=paste(round(trafficTowers$Dist[i], 2), "km"));}
par(mfrow=c(1,1));

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.rec[trafficTowers$TowerId[i],],
                        type="l", ylim=c(0, 2),
                        ylab="", main=round(trafficTowers$Dist[i], 2));}
par(mfrow=c(1,1));


avgMove.send <- apply(subDists.send, 2, mean);

setwd("c:/Data/NetworkResearch/Results/");
pdf("sendDists.pdf", height=6, width=6);
for ( i in 1:301 ) {
  data <- subDists.send[i,];
  print(plot(data, type="l", ylim=c(0, 3),
             ylab="", main=paste(round(trafficTowers$Dist[i], 2), "km")));
}
dev.off();

setwd("c:/Data/NetworkResearch/Results/");
pdf("recDists.pdf", height=6, width=6);
for ( i in 1:301 ) {
  data <- subDists.rec[i,];
  print(plot(data, type="l", ylim=c(0, 3),
             ylab="", main=paste(round(trafficTowers$Dist[i], 2), "km")));
}
dev.off();


TimeAxis <- function() {
  xpts <- c(6, 12, 18, 24, 30, 36, 42);
  xlabs <- c( "10:30 AM", "11:00 AM", "11:30 AM", "12:00 PM", "12:30 PM",
              "1:00 PM", "1:30 PM" );
  scale_x_continuous( breaks=xpts, labels=xlabs );
}

EventLine <- function() {
  geom_vline(xintercept=21, col="red");
}

sendDistPlot <- ggplot(fullDists, mapping=aes(time, send)) + geom_line() +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change");

receiveDistPlot <- ggplot(fullDists, mapping=aes(time, receive)) + geom_line() +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change");

df.send$Type <- "Sending";
df.rec$Type <- "Receiving";

df.latent <- rbind(df.send, df.rec);

tower1LatentPlot <- ggplot(df.latent[df.latent$Tower==1,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 1: 0.1 km");

tower2LatentPlot <- ggplot(df.latent[df.latent$Tower==2,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 2: 0.2 km");

tower3LatentPlot <- ggplot(df.latent[df.latent$Tower==3,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 3: 0.9 km");

tower4LatentPlot <- ggplot(df.latent[df.latent$Tower==4,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 4: 0.9 km");

tower5LatentPlot <- ggplot(df.latent[df.latent$Tower==5,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 5: 1.0 km");

ggplot(df.latent[df.latent$Tower==6,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 6: 1.2 km");

setwd("~/Dropbox/Chad/UW/NetworkResearch/svn/doc/Presentations/Darpa201407/Plots");
ggsave( "SendingLatentDistanceChange.png", height=6, width=9, plot=sendDistPlot );
ggsave( "ReceivingLatentDistanceChange.png", height=6, width=9, plot=receiveDistPlot );
ggsave( "Tower1LatentDistanceChange.png", height=6, width=9, plot=tower1LatentPlot );
ggsave( "Tower2LatentDistanceChange.png", height=6, width=9, plot=tower2LatentPlot );
ggsave( "Tower3LatentDistanceChange.png", height=6, width=9, plot=tower3LatentPlot );
ggsave( "Tower4LatentDistanceChange.png", height=6, width=9, plot=tower4LatentPlot );








Xarray <- BuildSnapshotArray( 11, 12, 6, 11, 30, 5, 1, 6, 1139 );

ldim <- 5;
lambdas <- c(0.001, 0.01, 0.1, 1, 10);
maxiter <- 100;
epsilon <- 1e-06

Rprof();

result <- Tmf.ALS(Xarray, ldim, lambda, NULL, NULL, 5, epsilon);

summaryRprof();

results <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  results[[resname]] <- Tmf.ALS(Xarray, ldim, lambda,
                                NULL, NULL, maxiter,epsilon);
  print(paste(resname, "finished"));
  timestamp();
}

res.01 <- results$lambda0.01;

Xarray2 <- BuildSnapshotArray( 11, 12, 6, 6, 0, 5, 1, 12*10, 1139 );

longResults <- Tmf.ALS(Xarray2, ldim, .01,
                       NULL, NULL, maxiter, epsilon);






SimulateTmfData <- function(ldim, N, T, latentSd=.1, Xsd=.1) {
  L <- R <- array(data=0, dim=c(N, ldim, T))
  L[,,1] <- rnorm(N*ldim, 0, latentSd);
  R[,,1] <- rnorm(N*ldim, 0, latentSd);
  for ( s in 2:T ) {
    L[,,s] <- rnorm(N*ldim, L[,,s-1], latentSd);
    R[,,s] <- rnorm(N*ldim, R[,,s-1], latentSd);
  }
  X <- array(data=0, dim=c(N, N, T));
  for ( s in 1:T ) {
    X[,,s] <- rnorm(N^2, L[,,s] %*% t(R[,,s]), Xsd);
  }
  list(X=X, L=L, R=R);
}

lambdas <- c(0.001, 0.01, 0.1, 1, 10);
maxiter <- 100;
epsilon <- 1e-6;
ldim <- 5;
N <- 100;
T <- 20;

simData <- SimulateTmfData(ldim, N, T);

Tmf.Objective(simData$L, simData$R, simData$X, 0, 0);
## 2010.128
Tmf.Objective(simData$L, simData$R, simData$X, 0.001, 0.001);
## 2010.227
Tmf.Objective(simData$L, simData$R, simData$X, 0.01, 0.01);
## 2011.112
Tmf.Objective(simData$L, simData$R, simData$X, 0.1, 0.1);
## 2019.962
Tmf.Objective(simData$L, simData$R, simData$X, 1, 1);
## 2108.465
Tmf.Objective(simData$L, simData$R, simData$X, 10, 10);
## 2993.493

Tmf.RMSE(simData$L, simData$R, simData$X);
## 0.1002529

L0 <- array(data=rnorm(5*100*20, 0, .1), dim=c(N, ldim, T));
R0 <- array(data=rnorm(5*100*20, 0, .1), dim=c(N, ldim, T));

simResults <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  simResults[[resname]] <- Tmf.ALS(simData$X, ldim, lambda,
                                NULL, NULL, maxiter, epsilon);
  print(paste(resname, "finished"));
  timestamp();
}

simData2 <- SimulateTmfData(ldim, N, T, 1, .1);

simResults2 <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  simResults2[[resname]] <- Tmf.ALS(simData2$X, ldim, lambda,
                                NULL, NULL, maxiter, epsilon);
  print(paste(resname, "finished"));
  timestamp();
}


Tmf.Objective(simData2$L, simData2$R, simData2$X, 0, 0);
## 2004.886
Tmf.Objective(simData2$L, simData2$R, simData2$X, 0.001, 0.001);
## 2014.956
Tmf.Objective(simData2$L, simData2$R, simData2$X, 0.01, 0.01);
## 2105.59
Tmf.Objective(simData2$L, simData2$R, simData2$X, 0.1, 0.1);
## 3011.928
Tmf.Objective(simData2$L, simData2$R, simData2$X, 1, 1);
## 12075.31
Tmf.Objective(simData2$L, simData2$R, simData2$X, 10, 10);
## 102709.1

Tmf.RMSE(simData2$L, simData2$R, simData2$X);
## 0.1001221

simResults <- Tmf.ALS(simData$X, ldim, lambda, NULL, NULL, maxiter, epsilon);

simResults2 <- Tmf.ALS(simData$X, ldim, lambda, simData$L, simData$R, maxiter, epsilon);

lambdas <- c(0.001, 0.01, 0.1, 1, 10);
maxiter <- 100;
epsilon <- 1e-6;
ldim <- 5;
N <- 100;
T <- 500;

simDataLong <- SimulateTmfData(ldim, N, T, .1, 1);

simResultsLong <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  simResultsLong[[resname]] <- Tmf.ALS(simDataLong$X, ldim, lambda,
                                NULL, NULL, maxiter, epsilon);
  print(paste(resname, "finished"));
  timestamp();
}

simResultsLong[["lambda100true"]] <- Tmf.ALS(simDataLong$X, ldim, 100,
                                            simDataLong$L, simDataLong$R,
                                            maxiter, epsilon);


Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 0, 0);
## 4997877
Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 0.001, 0.001);
## 4997879
Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 0.01, 0.01);
## 4997902
Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 0.1, 0.1);
## 4998127
Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 1, 1);
## 5000378
Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 10, 10);
## 5022891
Tmf.Objective(simDataLong$L, simDataLong$R, simDataLong$X, 100, 100);
## 5248014

Tmf.RMSE(simDataLong$L, simDataLong$R, simDataLong$X);
## 0.9997877




GetHoldout <- function( X, frac ) {
  Xho <- array(data=0, dim=dim(X));
  Nho <- floor(dim(X)[1]*dim(X)[2]*dim(X)[3]*frac);
  hoIndices <- cbind( sample( dim(X)[1], Nho, replace=TRUE ),
                      sample( dim(X)[2], Nho, replace=TRUE ),
                      sample( dim(X)[3], Nho, replace=TRUE ) );
  Xho[hoIndices] <- X[hoIndices];
  X[hoIndices] <- 0;
  list( training=X, holdout=Xho );
}

simDataLong$Holdout <- GetHoldout(simDataLong$X, 0.1);






dist1.true <- sapply(2:T, function(s) {
  sqrt(sum((simDataLong$L[1,,s] - simDataLong$L[1,,s-1])^2))
});

dist1.sim100 <- sapply(2:T, function(s) {
  sqrt(sum((simResultsLong$lambda100$L[1,,s] - simResultsLong$lambda100$L[1,,s-1])^2))
});

dist50.true <- sapply(2:T, function(s) {
  sqrt(sum((simDataLong$L[50,,s] - simDataLong$L[50,,s-1])^2))
});

dist50.sim100 <- sapply(2:T, function(s) {
  sqrt(sum((simResultsLong$lambda100$L[50,,s] - simResultsLong$lambda100$L[50,,s-1])^2))
});







testL <- Ldf.adj(res2$L, Xsub2$origRows, 790);

ggplot(testL, aes(L1, L2, color=towerId)) + geom_point();

rotL <- Ldf.adj.rot(res2$L, 1:1139, 790, 405);
rotL2 <- Ldf.adj.rot(res2.2$L, 1:1139, 790, 405);

rotComb <- rotL;
rotComb$L1end <- rotL2$L1;
rotComb$L2end <- rotL2$L2;

ggplot(rotComb, aes(L1, L2, color=towerId)) +
  geom_segment(aes(xend=L1end, yend=L2end), arrow=arrow(length=unit(0.3, "cm")))


ggplot(rotL, aes(L1, L2, color=towerId)) + geom_point()
ggplot(rotL2, aes(L1, L2, color=towerId)) + geom_point()

rotL3 <- Ldf.adj.rot(res3$L, Xsub3$origRows, 790, 405);
ggplot(rotL3, aes(L1, L2, color=towerId)) + geom_point();


ggplot(df, aes(x,y)) + geom_point()

dev.new()



rot1 <- Ldf.adj.rot(results$snap1$lambda10$L, 1:1139, 790, 405);
rot1.wz <- Ldf.adj.rot(results.wz$snap1$lambda10$L, 1:1139, 790, 405);
rot2 <- Ldf.adj.rot(results$snap2$lambda10$L, 1:1139, 790, 405);
rot3 <- Ldf.adj.rot(results$snap3$lambda10$L, 1:1139, 790, 405);
rot4 <- Ldf.adj.rot(results$snap4$lambda10$L, 1:1139, 790, 405);
rot5 <- Ldf.adj.rot(results$snap5$lambda10$L, 1:1139, 790, 405);

towerLocs <- TowerLocations();



eventLat <- towerLocs[790, "Lat"];
eventLong <- towerLocs[790, "Long"];

eventTowers <- TowersByDist(eventLat, eventLong, towerLocs);
distances <- eventTowers$Dist[order(eventTowers$TowerId)][1:1139];
rot1$Dist <- distances;
rot2$Dist <- distances;
rot3$Dist <- distances;
rot4$Dist <- distances;
rot5$Dist <- distances;


ggplot(rot1, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rot2, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rot3, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rot4, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rot5, aes(L1, L2, color=Dist)) + geom_point()


dfCombined <- rot1;
colnames(dfCombined) <- c("TowerId", "L1.1", "L2.1", "Dist");
dfCombined$L1.2 <- rot2$L1;
dfCombined$L2.2 <- rot2$L2;
dfCombined$L1.wz <- rot1.wz$L1;
dfCombined$L2.wz <- rot1.wz$L2;
dfCombined$L1.3 <- rot3$L1;
dfCombined$L2.3 <- rot3$L2;
dfCombined$L1.4 <- rot4$L1;
dfCombined$L2.4 <- rot4$L2;
dfCombined$L1.5 <- rot5$L1;
dfCombined$L2.5 <- rot5$L2;
dfCombined$LatentDist1 <- sqrt(rot1$L1^2 + rot1$L2^2);
dfCombined$LatentDist2 <- sqrt(rot2$L1^2 + rot2$L2^2);
dfCombined$LatentDist3 <- sqrt(rot3$L1^2 + rot3$L2^2);
dfCombined$LatentDist4 <- sqrt(rot4$L1^2 + rot4$L2^2);
dfCombined$LatentDist5 <- sqrt(rot5$L1^2 + rot5$L2^2);

dfOriginal <- dfCombined;
dfOriginal$L1.1 <- results$snap1$lambda10$L[,1];
dfOriginal$L2.1 <- results$snap1$lambda10$L[,2];
dfOriginal$L1.2 <- results$snap2$lambda10$L[,1];
dfOriginal$L2.2 <- results$snap2$lambda10$L[,2];
dfOriginal$L1.3 <- results$snap3$lambda10$L[,1];
dfOriginal$L2.3 <- results$snap3$lambda10$L[,2];
dfOriginal$L1.wz <- results.wz$snap1$lambda10$L[,1];
dfOriginal$L2.wz <- results.wz$snap1$lambda10$L[,2];


plot(density(dfCombined$LatentDist1));
plot(density(dfCombined$LatentDist2));
plot(density(dfCombined$LatentDist3));
plot(density(dfCombined$LatentDist4));
plot(density(dfCombined$LatentDist5));

plot(density(dfCombined$LatentDist1 - dfCombined$LatentDist2));
plot(density(dfCombined$LatentDist1 - dfCombined$LatentDist3));
plot(density(dfCombined$LatentDist2 - dfCombined$LatentDist3));
plot(density(dfCombined$LatentDist1 - dfCombined$LatentDist4));


ggplot(dfCombined, aes(L1.1, L2.1, color=Dist)) +
  geom_segment(aes(xend=L1.2, yend=L2.2), arrow=arrow(length=unit(0.3, "cm")))

ggplot(dfCombined, aes(L1.1, L2.1, color=Dist)) +
  geom_segment(aes(xend=L1.3, yend=L2.3), arrow=arrow(length=unit(0.3, "cm")))


xrng <- c(-5, 10);
yrng <- c(-3, 8);
ggplot(dfOriginal, aes(L1.1, L2.1, color=Dist)) + geom_point() +
  xlim(xrng) + ylim(yrng);
ggplot(dfOriginal, aes(L1.2, L2.2, color=Dist)) + geom_point() +
  xlim(xrng) + ylim(yrng);
ggplot(dfOriginal, aes(L1.3, L2.3, color=Dist)) + geom_point() +
  xlim(xrng) + ylim(yrng);

ggplot(dfOriginal, aes(L1.1, L2.1, color=Dist)) +
  geom_segment(aes(xend=L1.2, yend=L2.2), arrow=arrow(length=unit(0.3, "cm")))

ggplot(dfOriginal, aes(L1.1, L2.1, color=Dist)) +
  geom_segment(aes(xend=L1.3, yend=L2.3), arrow=arrow(length=unit(0.3, "cm")))





rot1b <- Ldf.adj.rot(results$snap1$lambda10b$L, 1:1139, 790, 405);
rot2b <- Ldf.adj.rot(results$snap2$lambda10b$L, 1:1139, 790, 405);
rot3b <- Ldf.adj.rot(results$snap3$lambda10b$L, 1:1139, 790, 405);
rot1b$Dist <- distances;
rot2b$Dist <- distances;
rot3b$Dist <- distances;


ggplot(rot1b, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rot2b, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rot3b, aes(L1, L2, color=Dist)) + geom_point()


dfCombinedb <- rot1b;
colnames(dfCombinedb) <- c("TowerId", "L1.1", "L2.1", "Dist");
dfCombinedb$L1.2 <- rot2b$L1;
dfCombinedb$L2.2 <- rot2b$L2;
dfCombinedb$L1.3 <- rot3b$L1;
dfCombinedb$L2.3 <- rot3b$L2;
dfCombinedb$LatentDist1 <- sqrt(rot1b$L1^2 + rot1b$L2^2);
dfCombinedb$LatentDist2 <- sqrt(rot2b$L1^2 + rot2b$L2^2);
dfCombinedb$LatentDist3 <- sqrt(rot3b$L1^2 + rot3b$L2^2);

plot(density(dfCombinedb$LatentDist1));
plot(density(dfCombinedb$LatentDist2));
plot(density(dfCombinedb$LatentDist3));




## combining multiple snapshots

combXpre <- CombineSnapshots( 11, 12, 6, 11, 0, 5, 6 );
combXdur <- CombineSnapshots( 11, 12, 6, 11, 45, 5, 6 );
combXpost <- CombineSnapshots( 11, 12, 6, 14, 30, 5, 6 );

ldim <- 2;
lambdas <- c(0.001, 0.01, 0.1, 1, 10, 100);
maxiter <- 1000;
epsilon <- 1e-06;

als.pre <- als.dur <- als.post <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  als.pre[[resname]] <- ALS(combXpre, ldim, lambda,
                            NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " pre", sep=""));
  timestamp(); 
  als.dur[[resname]] <- ALS(combXdur, ldim, lambda,
                            NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " dur", sep=""));
  timestamp(); 
  als.post[[resname]] <- ALS(combXpost, ldim, lambda,
                             NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " post", sep=""));
  timestamp(); 
}



rotpre <- Ldf.adj.rot(als.pre$lambda100$L, 1:1139, 790, 405);
rotdur <- Ldf.adj.rot(als.dur$lambda100$L, 1:1139, 790, 405);
rotpost <- Ldf.adj.rot(als.post$lambda100$L, 1:1139, 790, 405);
rotpre$Dist <- distances;
rotdur$Dist <- distances;
rotpost$Dist <- distances;

ggplot(rotpre, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rotdur, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rotpost, aes(L1, L2, color=Dist)) + geom_point()


dfCombinedb <- rotpre;
colnames(dfCombinedb) <- c("TowerId", "L1.pre", "L2.pre", "Dist");
dfCombinedb$L1.dur <- rotdur$L1;
dfCombinedb$L2.dur <- rotdur$L2;
dfCombinedb$L1.post <- rotpost$L1;
dfCombinedb$L2.post <- rotpost$L2;
dfCombinedb$LatentDistPre <- sqrt(rotpre$L1^2 + rotpre$L2^2);
dfCombinedb$LatentDistDur <- sqrt(rotdur$L1^2 + rotdur$L2^2);
dfCombinedb$LatentDistPost <- sqrt(rotpost$L1^2 + rotpost$L2^2);

plot(density(dfCombinedb$LatentDistPre));
plot(density(dfCombinedb$LatentDistDur));
plot(density(dfCombinedb$LatentDistPost));




combXpre <- CombineSnapshots( 11, 12, 6, 11, 0, 5, 6 );
combXdur <- CombineSnapshots( 11, 12, 6, 11, 45, 5, 6 );

ldim <- 5;
lambdas <- c(10, 100);
maxiter <- 500;
epsilon <- 1e-06;

als.pre.5d <- als.dur.5d <- als.post.5d <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  als.pre.5d[[resname]] <- ALS(combXpre, ldim, lambda,
                            NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " pre", sep=""));
  timestamp(); 
  als.dur.5d[[resname]] <- ALS(combXdur, ldim, lambda,
                            NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " dur", sep=""));
  timestamp(); 
  als.post.5d[[resname]] <- ALS(combXpost, ldim, lambda,
                             NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " post", sep=""));
  timestamp(); 
}

presums <- apply(combXpre, 1, sum);

distBySum <- distances[order(presums, decreasing=TRUE)]


rotpre.5d <- Ldf.adj.rot(als.pre.5d$lambda100$L, 1:1139, 790, 405);
rotdur.5d <- Ldf.adj.rot(als.dur.5d$lambda100$L, 1:1139, 790, 405);
rotpost.5d <- Ldf.adj.rot(als.post.5d$lambda100$L, 1:1139, 790, 405);
rotpre.5d$Dist <- distances;
rotdur.5d$Dist <- distances;
rotpost.5d$Dist <- distances;


ggplot(rotpre.5d, aes(L1, L2, color=Dist)) + geom_point()
ggplot(rotpre.5d, aes(L1, L3, color=Dist)) + geom_point()
ggplot(rotpre.5d, aes(L1, L4, color=Dist)) + geom_point()
ggplot(rotpre.5d, aes(L1, L5, color=Dist)) + geom_point()
ggplot(rotpre.5d, aes(L2, L5, color=Dist)) + geom_point()


DistanceMatrix <- function(L) {
  apply(L, 1, function(l1) {
    apply(L, 1, function(l2) {
      sqrt(sum((l1-l2)^2));
    });
  });
}

L.pre <- als.pre.5d$lambda100$L;
L.pre <- L.pre[order(distances, decreasing=TRUE),];
L.dur <- als.dur.5d$lambda100$L;
L.dur <- L.dur[order(distances, decreasing=TRUE),];
L.post <- als.post.5d$lambda100$L;
L.post <- L.post[order(distances, decreasing=TRUE),];

dist.5d.pre <- DistanceMatrix(L.pre);
dist.5d.dur <- DistanceMatrix(L.dur);
dist.5d.post <- DistanceMatrix(L.post);

plot(density(dist.5d.pre - dist.5d.dur))
plot(density(dist.5d.pre - dist.5d.post))

presums <- apply(combXpre, 1, sum);
best200 <- order(presums, decreasing=TRUE)[1:200];
bestDist <- distances[best200];

dursums <- apply(combXdur, 1, sum);



df.dist.5d.pre <- melt(dist.5d.pre);

df.dist.5d.diff <- melt(dist.5d.pre - dist.5d.dur);

ggplot(df.dist.5d.pre, aes(X1,X2, fill=value)) + geom_raster()

ggplot(df.dist.5d.diff, aes(X1,X2, fill=value)) + geom_raster()

heatmap(dist.5d.pre)


subset.pre <- als.pre.5d$lambda100$L[best200,];
subset.pre <- subset.pre[order(bestDist),];

dist.subset.pre <- DistanceMatrix(subset.pre);

df.dist.subset.pre <- melt(dist.subset.pre);

subset.dur <- als.dur.5d$lambda100$L[best200,];
subset.dur <- subset.dur[order(bestDist),];

dist.subset.dur <- DistanceMatrix(subset.dur);

df.dist.subset.dur <- melt(dist.subset.dur);

subset.post <- als.post.5d$lambda100$L[best200,];
subset.post <- subset.post[order(bestDist),];

dist.subset.post <- DistanceMatrix(subset.post);

df.dist.subset.post <- melt(dist.subset.post);




df.dist.subset.diff <- melt(dist.subset.pre - dist.subset.dur);
df.dist.subset.diff2 <- melt(dist.subset.pre - dist.subset.post);
df.dist.subset.diff3 <- melt(dist.subset.post - dist.subset.dur);

ggplot(df.dist.subset.pre, aes(X1,X2, fill=value)) + geom_raster()

bombingLatentDistChgPlot <- ggplot(df.dist.subset.diff, aes(X1,X2, fill=value)) +
  geom_raster() +
  scale_fill_gradient2(low="red", mid="gray", high="blue", guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)


ggplot(df.dist.subset.diff2, aes(X1,X2, fill=value)) + geom_raster() +
  scale_fill_gradient2(low="red", mid="black", high="blue") +
  labs(title="pre-post")

ggplot(df.dist.subset.diff3, aes(X1,X2, fill=value)) + geom_raster() +
  scale_fill_gradient2(low="red", mid="black", high="blue") +
  labs(title="post-dur")

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave("BombingLatentDistanceChange.png", height=5, width=5, plot=bombingLatentDistChgPlot);


setwd("c:/Data/Temp/");
load("inaugTMF.rdata");

inaugNetData <- BuildSnapshotArray( 11, 12, 15, 7, 0, 5, 1, 36, 1139 );

inaugFrob <- sapply(1:dim(inaugTMF$L)[3], function(t) {
  UVhat <- inaugTMF$L[,,t] %*% t(inaugTMF$R[,,t]);
  sqrt(sum(diag(UVhat %*% t(UVhat))));
});

latentDists.send <- matrix(data=0, nrow=dim(inaugTMF$L)[1], ncol=dim(inaugTMF$L)[3]-1);
for ( i in 2:dim(inaugTMF$L)[3] ) {
  for ( j in 1:dim(inaugTMF$L)[1] ) {
    latentDists.send[j,i-1] <- sqrt(sum((inaugTMF$L[j,,i]-inaugTMF$L[j,,i-1])^2));
  }
}

latentDists.rec <- matrix(data=0, nrow=dim(inaugTMF$R)[1], ncol=dim(inaugTMF$R)[3]-1);
for ( i in 2:dim(inaugTMF$L)[3] ) {
  for ( j in 1:dim(inaugTMF$L)[1] ) {
    latentDists.rec[j,i-1] <- sqrt(sum((inaugTMF$R[j,,i]-inaugTMF$R[j,,i-1])^2));
  }
}

par(mfrow=c(5,5));
for ( i in sample(1139, 25) ) {plot(latentDists.send[i,], type="l");}
par(mfrow=c(1,1));

plot(latentDists.send[405,], type="l");
plot(latentDists.send[790,], type="l");

fullLatentDists.send <- sapply(2:dim(inaugTMF$L)[3], function(i) {
  sqrt(sum((inaugTMF$L[,,i]-inaugTMF$L[,,i-1])^2));
});

fullLatentDists.rec <- sapply(2:dim(inaugTMF$R)[3], function(i) {
  sqrt(sum((inaugTMF$R[,,i]-inaugTMF$R[,,i-1])^2));
});

plot(fullLatentDists.send, type="l")
plot(fullLatentDists.rec, type="l")

fullDists <- data.frame(time=1:35, send=fullLatentDists.send,
                        receive=fullLatentDists.rec);



towerLocs <- TowerLocations();

eventLat <- 34.519;
eventLong <- 69.194;

eventTowers <- TowersByDist(eventLat, eventLong, towerLocs);


par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.send[eventTowers$TowerId[i],], type="l", ylim=c(0, 2));}
par(mfrow=c(1,1));

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.rec[eventTowers$TowerId[i],], type="l", ylim=c(0, 2));}
par(mfrow=c(1,1));

towerCalls <- apply(inaugNetData, 1, sum);

highTrafficTowers <- (1:1139)[towerCalls >= 750];
evTowers2 <- eventTowers[order(eventTowers$TowerId),];
trafficTowers <- evTowers2[highTrafficTowers,];
trafficTowers <- trafficTowers[order(trafficTowers$Dist),];

subDists.send <- latentDists.send[trafficTowers$TowerId,];
subDists.rec <- latentDists.rec[trafficTowers$TowerId,];

df.send <- melt(subDists.send);
colnames(df.send) <- c("Tower", "Timepoint", "DistMove");
df.send$Dist <- trafficTowers$Dist[df.send$Tower];

df.rec <- melt(subDists.rec);
colnames(df.rec) <- c("Tower", "Timepoint", "DistMove");
df.rec$Dist <- trafficTowers$Dist[df.rec$Tower];

ggplot(data=df.send, aes(Timepoint, Tower, fill=DistMove)) + geom_raster();

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.send[trafficTowers$TowerId[i],],
                        type="l", ylim=c(0, 2),
                        ylab="", main=paste(round(trafficTowers$Dist[i], 2), "km"));}
par(mfrow=c(1,1));

par(mfrow=c(5,5));
for ( i in 1:25 ) {plot(latentDists.rec[trafficTowers$TowerId[i],],
                        type="l", ylim=c(0, 2),
                        ylab="", main=round(trafficTowers$Dist[i], 2));}
par(mfrow=c(1,1));


avgMove.send <- apply(subDists.send, 2, mean);

setwd("c:/Data/NetworkResearch/Results/");
pdf("sendDists-inaug.pdf", height=6, width=6);
for ( i in 1:301 ) {
  data <- subDists.send[i,];
  print(plot(data, type="l", ylim=c(0, 3),
             ylab="", main=paste(round(trafficTowers$Dist[i], 2), "km")));
}
dev.off();

setwd("c:/Data/NetworkResearch/Results/");
pdf("recDists-inaug.pdf", height=6, width=6);
for ( i in 1:301 ) {
  data <- subDists.rec[i,];
  print(plot(data, type="l", ylim=c(0, 3),
             ylab="", main=paste(round(trafficTowers$Dist[i], 2), "km")));
}
dev.off();

TimeAxis <- function() {
  xpts <- c(6, 12, 18, 24, 30, 36);
  xlabs <- c( "7:30 AM", "8:00 AM", "8:30 AM", "9:00 AM", "9:30 AM", "10:00 AM" );
  scale_x_continuous( breaks=xpts, labels=xlabs );
}

EventLine <- function() {
  geom_vline(xintercept=6, col="red");
}

sendDistPlot <- ggplot(fullDists, mapping=aes(time, send)) + geom_line() +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change");

receiveDistPlot <- ggplot(fullDists, mapping=aes(time, receive)) + geom_line() +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change");

df.send$Type <- "Sending";
df.rec$Type <- "Receiving";

df.latent <- rbind(df.send, df.rec);

tower1LatentPlot <- ggplot(df.latent[df.latent$Tower==1,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 1: 0.5 km");

tower2LatentPlot <- ggplot(df.latent[df.latent$Tower==2,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 2: 0.6 km");

tower3LatentPlot <- ggplot(df.latent[df.latent$Tower==3,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 3: 0.6 km");

tower4LatentPlot <- ggplot(df.latent[df.latent$Tower==4,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 4: 1.0 km");

tower5LatentPlot <- ggplot(df.latent[df.latent$Tower==5,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 5: 1.1 km");

ggplot(df.latent[df.latent$Tower==6,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 6: 1.2 km");

setwd("C:/Data/NetworkResearch/Results");
ggsave( "SendingLatentDistanceChange-inaug.png", height=6, width=9, plot=sendDistPlot );
ggsave( "ReceivingLatentDistanceChange-inaug.png", height=6, width=9, plot=receiveDistPlot );
ggsave( "Tower1LatentDistanceChange-inaug.png", height=6, width=9, plot=tower1LatentPlot );
ggsave( "Tower2LatentDistanceChange-inaug.png", height=6, width=9, plot=tower2LatentPlot );
ggsave( "Tower3LatentDistanceChange-inaug.png", height=6, width=9, plot=tower3LatentPlot );
ggsave( "Tower4LatentDistanceChange-inaug.png", height=6, width=9, plot=tower4LatentPlot );






sendDistPlot <- ggplot(fullDists, mapping=aes(time, send)) + geom_line() +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change");

receiveDistPlot <- ggplot(fullDists, mapping=aes(time, receive)) + geom_line() +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change");

df.send$Type <- "Sending";
df.rec$Type <- "Receiving";

df.latent <- rbind(df.send, df.rec);

tower1LatentPlot <- ggplot(df.latent[df.latent$Tower==1,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 1: 0.1 km");

tower2LatentPlot <- ggplot(df.latent[df.latent$Tower==2,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 2: 0.2 km");

tower3LatentPlot <- ggplot(df.latent[df.latent$Tower==3,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 3: 0.9 km");

tower4LatentPlot <- ggplot(df.latent[df.latent$Tower==4,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 4: 0.9 km");

tower5LatentPlot <- ggplot(df.latent[df.latent$Tower==5,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 5: 1.0 km");

ggplot(df.latent[df.latent$Tower==6,],
                         mapping=aes(Timepoint, DistMove, color=Type)) +
  geom_line() + scale_color_manual(values=c("blue", "darkgreen")) +
  TimeAxis() + EventLine() +
  labs(x="Time", y="Latent Distance Change", title="Tower 6: 1.2 km");

setwd("~/Dropbox/Chad/UW/NetworkResearch/svn/doc/Presentations/Darpa201407/Plots");
ggsave( "SendingLatentDistanceChange.png", height=6, width=9, plot=sendDistPlot );
ggsave( "ReceivingLatentDistanceChange.png", height=6, width=9, plot=receiveDistPlot );
ggsave( "Tower1LatentDistanceChange.png", height=6, width=9, plot=tower1LatentPlot );
ggsave( "Tower2LatentDistanceChange.png", height=6, width=9, plot=tower2LatentPlot );
ggsave( "Tower3LatentDistanceChange.png", height=6, width=9, plot=tower3LatentPlot );
ggsave( "Tower4LatentDistanceChange.png", height=6, width=9, plot=tower4LatentPlot );

## Mazar e Sharif
## 36.655091, 67.113895
##

latent.delta <- apply(dist.subset.pre - dist.subset.dur, 1, mean);


towerLocs[best200[order(bestDist)][(1:200)[latent.delta < -1.5]],]
##     TowerId    Long     Lat                           TowerList
## 790     790 69.1836 34.5172 11681:11682:11683:11684:11685:11686
## 405     405 69.1809 34.5178 28951:28952:28953:28954:28955:28956
## 950     950 69.1463 34.5512 11611:11612:11613:11614:11615:11616
## 6         6 67.1127 36.7113 40801:40802:40803:40804:40805:40806
## 424     424 67.1130 36.7132                   40421:40422:40423
## 76       76 67.1077 36.7131 40041:40042:40043:40044:40045:40046
## 884     884 67.1120 36.7236                   40501:40502:40503
## 737     737 62.2854 35.2452       20821:20822:20823:20825:20826

towerLocs[towerLocs$Long>67 & towerLocs$Long<67.5 & towerLocs$Lat>36.5 & towerLocs$Lat<37,]

eventTowers2 <- TowersByDist(36.7, 67.1, towerLocs);

cbind(eventTowers2$TowerId[1:30],
      dursums[eventTowers2$TowerId[1:30]] / presums[eventTowers2$TowerId[1:30]])



evT1 <- eventTowers[order(eventTowers$TowerId),]
evT2 <- eventTowers[order(eventTowers2$TowerId),]

minDists <- pmin(evT1$Dist, evT2$Dist);

df.dist.5d.diff$Dist1 <- minDists[df.dist.5d.diff$X1];
df.dist.5d.diff$Dist2 <- minDists[df.dist.5d.diff$X2];
df.dist.5d.diff$MinDist <- pmin(df.dist.5d.diff$Dist1, df.dist.5d.diff$Dist2);
df.dist.5d.diff$MaxDist <- pmax(df.dist.5d.diff$Dist1, df.dist.5d.diff$Dist2);
df.dist.5d.diff$MeanDist <- (df.dist.5d.diff$Dist1 + df.dist.5d.diff$Dist2) / 2;

lmdf <- df.dist.5d.diff[df.dist.5d.diff$X1 < df.dist.5d.diff$X2,];

summary(lm(value~Dist1, data=lmdf));
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.452e-02  1.160e-03   12.52   <2e-16 ***
## Dist1       7.741e-05  3.996e-06   19.37   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7332 on 648089 degrees of freedom
## Multiple R-squared:  0.0005788,	Adjusted R-squared:  0.0005773 
## F-statistic: 375.3 on 1 and 648089 DF,  p-value: < 2.2e-16

summary(lm(value~Dist2, data=lmdf));
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.713e-02  1.150e-03  23.594   <2e-16 ***
## Dist2       7.555e-06  4.070e-06   1.856   0.0634 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7335 on 648089 degrees of freedom
## Multiple R-squared:  5.317e-06,	Adjusted R-squared:  3.774e-06 
## F-statistic: 3.446 on 1 and 648089 DF,  p-value: 0.06342


Xdursub <- combXdur[best200,best200];
Xdursub <- Xdursub[order(bestDist),order(bestDist)];

Xpresub <- combXpre[best200,best200];
Xpresub <- Xpresub[order(bestDist),order(bestDist)];

Xdurord <- combXdur[order(distances), order(distances)];

callVolumeHeatmap <- ggplot(melt(Xdursub), aes(X1,X2, fill=log(value+1))) +
  geom_raster() +
  scale_fill_gradient2(low="black", mid="red", high="yellow", midpoint=3, guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

ggplot(melt(Xpresub), aes(X1,X2, fill=log(value+1))) +
  geom_raster() +
  scale_fill_gradient2(low="black", mid="red", high="yellow", midpoint=3, guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave("BombingCallVolumeHeatmap.png", height=5, width=5, plot=callVolumeHeatmap);




## inauguration

inaugXpre <- CombineSnapshots( 11, 12, 15, 7, 0, 5, 6 );
inaugXdur <- CombineSnapshots( 11, 12, 15, 8, 0, 5, 6 );

ldim <- 5;
lambda <- 100;
maxiter <- 500;
epsilon <- 1e-06;

inaug.pre.5d <- ALS(inaugXpre, ldim, lambda,
                    NULL, NULL, maxiter, epsilon);
inaug.dur.5d <- ALS(inaugXdur, ldim, lambda,
                    NULL, NULL, maxiter, epsilon);




inaugLat <- towerLocs[183, "Lat"];
inaugLong <- towerLocs[183, "Long"];

inaugTowers <- TowersByDist(inaugLat, inaugLong, towerLocs);
inaug.distances <- inaugTowers$Dist[order(inaugTowers$TowerId)][1:1139];

inaug.dursums <- apply(inaugXpre, 1, sum);
inaug.best200 <- order(inaug.dursums, decreasing=TRUE)[1:200];
inaug.bestDist <- inaug.distances[inaug.best200];

inaugXsub <- inaugXdur[inaug.best200,inaug.best200];
inaugXsub <- inaugXsub[order(inaug.bestDist),order(inaug.bestDist)];

ggplot(melt(inaugXsub), aes(X1,X2, fill=log(value+1))) +
  geom_raster() +
  scale_fill_gradient2(low="black", mid="red", high="yellow", midpoint=3, guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)







inaug.subset.pre <- inaug.pre.5d$L[inaug.best200,];
inaug.subset.pre <- inaug.subset.pre[order(inaug.bestDist),];

inaug.dist.subset.pre <- DistanceMatrix(inaug.subset.pre);

df.inaug.dist.subset.pre <- melt(inaug.dist.subset.pre);

inaug.subset.dur <- inaug.dur.5d$L[inaug.best200,];
inaug.subset.dur <- inaug.subset.dur[order(inaug.bestDist),];

inaug.dist.subset.dur <- DistanceMatrix(inaug.subset.dur);

df.inaug.dist.subset.dur <- melt(inaug.dist.subset.dur);


df.inaug.dist.subset.diff <- melt(inaug.dist.subset.pre - inaug.dist.subset.dur);

ggplot(df.dist.subset.pre, aes(X1,X2, fill=value)) + geom_raster()

inaugLatentDistChgPlot <- ggplot(df.inaug.dist.subset.diff, aes(X1,X2, fill=value)) +
  geom_raster() +
  scale_fill_gradient2(low="red", mid="gray", high="blue", guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave("InaugurationLatentDistanceChange.png", height=5, width=5, plot=inaugLatentDistChgPlot);

