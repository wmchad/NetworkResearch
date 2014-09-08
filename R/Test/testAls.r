require("reshape");
require("grid");
require("ggplot2");
require("Matrix");

## set.seed(12345)

setwd("c:/Code/NetworkResearch/R/Analysis/");
source("als.r");
setwd("c:/Code/NetworkResearch/R/Functions/Data/");
source("TowerFunctions.r");

CombineSnapshots <- function( year, month, day, hour, minute, nMin, nPeriods ) {
  X <- matrix(nrow=1139, ncol=1139, data=0);
  for ( p in 1:nPeriods ) {
    snap <- GetNetworkSnapshot2( year, month, day, hour, minute, nMin );
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

snap <- GetNetworkSnapshot2( 11, 12, 6, 11, 30, 5 );
snap2 <- GetNetworkSnapshot2( 11, 12, 6, 11, 35, 5 );
snap3 <- GetNetworkSnapshot2( 11, 12, 6, 11, 50, 5 );

snap4 <- GetNetworkSnapshot2( 11, 12, 6, 11, 55, 5 );
snap5 <- GetNetworkSnapshot2( 11, 12, 6, 11, 25, 5 );

X <- matrix(nrow=1139, ncol=1139, data=0);
for ( i in 1:nrow(snap) ) {
  if ( snap$FromTower[i] > 0 & snap$FromTower[i] <= 1139 &
       snap$ToTower[i] > 0 & snap$ToTower[i] <= 1139 ) {
    X[snap$FromTower[i], snap$ToTower[i]] <- snap$Calls[i];
  }
}

X2 <- matrix(nrow=1139, ncol=1139, data=0);
for ( i in 1:nrow(snap2) ) {
  if ( snap2$FromTower[i] > 0 & snap2$FromTower[i] <= 1139 &
       snap2$ToTower[i] > 0 & snap2$ToTower[i] <= 1139 ) {
    X2[snap2$FromTower[i], snap2$ToTower[i]] <- snap2$Calls[i];
  }
}

X3 <- matrix(nrow=1139, ncol=1139, data=0);
for ( i in 1:nrow(snap3) ) {
  if ( snap3$FromTower[i] > 0 & snap3$FromTower[i] <= 1139 &
       snap3$ToTower[i] > 0 & snap3$ToTower[i] <= 1139 ) {
    X3[snap3$FromTower[i], snap3$ToTower[i]] <- snap3$Calls[i];
  }
}

X4 <- matrix(nrow=1139, ncol=1139, data=0);
for ( i in 1:nrow(snap4) ) {
  if ( snap4$FromTower[i] > 0 & snap4$FromTower[i] <= 1139 &
       snap4$ToTower[i] > 0 & snap4$ToTower[i] <= 1139 ) {
    X4[snap4$FromTower[i], snap4$ToTower[i]] <- snap4$Calls[i];
  }
}

X5 <- matrix(nrow=1139, ncol=1139, data=0);
for ( i in 1:nrow(snap5) ) {
  if ( snap5$FromTower[i] > 0 & snap5$FromTower[i] <= 1139 &
       snap5$ToTower[i] > 0 & snap5$ToTower[i] <= 1139 ) {
    X5[snap5$FromTower[i], snap5$ToTower[i]] <- snap5$Calls[i];
  }
}

Xsub <- X;

PrepareForALS <- function(X, ldim) {
  Xsub <- X;
  rowCounts <- apply(Xsub, 1, function(x){sum(x>0);});
  colCounts <- apply(Xsub, 2, function(x){sum(x>0);});
  keptRows <- 1:nrow(Xsub);
  keptCols <- 1:ncol(Xsub);

  while ( sum(rowCounts < ldim) > 0 || sum(colCounts < ldim) ) {
    keptRows <- keptRows[rowCounts >= ldim];
    keptCols <- keptCols[colCounts >= ldim];
    Xsub <- Xsub[rowCounts >= ldim,];
    Xsub <- Xsub[,colCounts >= ldim];
  
    rowCounts <- apply(Xsub, 1, function(x){sum(x>0);});
    colCounts <- apply(Xsub, 2, function(x){sum(x>0);});
  }
  list(X=Xsub, origRows=keptRows, origCols=keptCols);
}


Xsub2 <- PrepareForALS(X, ldim);
Xsub2.2 <- PrepareForALS(X2, ldim);

ldim <- 2;
lambdas <- c(0.001, 0.01, 0.1, 1, 10);
maxiter <- 1000;
epsilon <- 1e-06;

results <- NULL;
results$snap1 <- NULL;
results$snap2 <- NULL;
results$snap3 <- NULL;
results$snap4 <- NULL;
results$snap5 <- NULL;
results$snap1init <- NULL;
results$snap2init <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  results$snap1[[resname]] <- ALS(X, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 1", sep=""));
  timestamp(); 
  results$snap2[[resname]] <- ALS(X2, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 2", sep=""));
  timestamp(); 
  results$snap3[[resname]] <- ALS(X3, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 3", sep=""));
  timestamp(); 
  results$snap4[[resname]] <- ALS(X4, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 4", sep=""));
  timestamp(); 
  results$snap5[[resname]] <- ALS(X5, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 5", sep=""));
  timestamp(); 
  results$snap1init[[resname]] <- ALS(X, ldim, lambda,
                                      results$snap2[[resname]]$L,
                                      results$snap2[[resname]]$R,
                                      maxiter, epsilon);
  print(paste("Finished ", resname, " snap 1 initialized", sep=""));
  timestamp(); 
  results$snap2init[[resname]] <- ALS(X2, ldim, lambda,
                                      results$snap1[[resname]]$L,
                                      results$snap1[[resname]]$R,
                                      maxiter, epsilon);
  print(paste("Finished ", resname, " snap 2 initialized", sep=""));
  timestamp(); 
}

results.wz <- NULL;
results.wz$snap1 <- NULL;
results.wz$snap2 <- NULL;
results.wz$snap1init <- NULL;
results.wz$snap2init <- NULL;

for ( lambda in lambdas ) {
  resname <- paste("lambda", lambda, sep="");
  results.wz$snap1[[resname]] <- ALSwithZeroes(X, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 1", sep=""));
  timestamp(); 
  results.wz$snap2[[resname]] <- ALSwithZeroes(X2, ldim, lambda,
                                  NULL, NULL, maxiter, epsilon);
  print(paste("Finished ", resname, " snap 2", sep=""));
  timestamp(); 
  results.wz$snap1init[[resname]] <- ALSwithZeroes(X, ldim, lambda,
                                      results.wz$snap2[[resname]]$L,
                                      results.wz$snap2[[resname]]$R,
                                      maxiter, epsilon);
  print(paste("Finished ", resname, " snap 1 initialized", sep=""));
  timestamp(); 
  results.wz$snap2init[[resname]] <- ALSwithZeroes(X2, ldim, lambda,
                                      results.wz$snap1[[resname]]$L,
                                      results.wz$snap1[[resname]]$R,
                                      maxiter, epsilon);
  print(paste("Finished ", resname, " snap 2 initialized", sep=""));
  timestamp(); 
}


Xsub3 <- PrepareForALS(X, 3);
res3 <- ALS(Xsub3$X, 3, lambda, maxiter, epsilon);

Ladj <- apply(res$L, 1, function(l) {l - res$L[keptRows==790,];});

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
save(df.dist.subset.diff, file="df.dist.subset.diff.rdata")

ggsave("BombingLatentDistanceChange.png", height=5, width=5, plot=bombingLatentDistChgPlot);



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

Xdurdf <- melt(Xdursub);

callVolumeHeatmap <- ggplot(Xdurdf, aes(X1,X2, fill=log(value+1))) +
  geom_raster() +
  scale_fill_gradient2("", low="grey", mid="darkblue", high="blue", midpoint=3) +
  guides( fill=guide_colorbar(label=FALSE, barwidth=2, barheight=20) ) +
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

ggsave("BombingCallVolumeHeatmap.png", height=5, width=6.5, plot=callVolumeHeatmap);




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
  scale_fill_gradient2(low="red", mid="gray", high="blue", guide=FALSE, limits=c(-20, 20)) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");
save(df.inaug.dist.subset.diff, file="df.inaug.dist.subset.diff.rdata");

ggsave("InaugurationLatentDistanceChange.png", height=5, width=5, plot=inaugLatentDistChgPlot);










####################################
towerOrder <- best200[order(bestDist)];

index790 <- (1:200)[towerOrder==790];
index337 <- (1:200)[towerOrder==337];


Tower790BombingPlot <- ggplot(df.dist.subset.diff[df.dist.subset.diff$X2==index790,],
                            aes(X1,X2, fill=-value)) +
  geom_raster() +
  scale_fill_gradient2("", low="red", mid="gray", high="blue", guide=FALSE, limits=c(-20, 20),
                       breaks=c(-20, 0, 20)) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), text=element_text(size=20)) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

Tower337BombingPlot <- ggplot(df.dist.subset.diff[df.dist.subset.diff$X2==index337,],
                           aes(X1,X2, fill=-value)) +
  geom_raster() +
  scale_fill_gradient2("", low="red", mid="gray", high="blue", guide=FALSE, limits=c(-20, 20),
                       breaks=c(-20, 0, 20)) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), text=element_text(size=20)) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");
ggsave("Tower790BombingPlot.png", height=.7, width=5, plot=Tower790BombingPlot);
ggsave("Tower337BombingPlot.png", height=.7, width=5, plot=Tower337BombingPlot);
