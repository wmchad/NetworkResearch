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

DistanceMatrix <- function(L) {
  apply(L, 1, function(l1) {
    apply(L, 1, function(l2) {
      sqrt(sum((l1-l2)^2));
    });
  });
}



## bombing

bombXpre <- CombineSnapshots( 11, 12, 6, 11, 0, 5, 6 );
bombXdur <- CombineSnapshots( 11, 12, 6, 11, 45, 5, 6 );

ldim <- 5;
lambda <- 100;
maxiter <- 500;
epsilon <- 1e-06;

bomb.pre.5d <- ALS(bombXpre, ldim, lambda,
                    NULL, NULL, maxiter, epsilon);
print("finished1");
bomb.dur.5d <- ALS(bombXdur, ldim, lambda,
                    NULL, NULL, maxiter, epsilon);
print("finished");



towerLocs <- TowerLocations();

bombLat <- towerLocs[790, "Lat"];
bombLong <- towerLocs[790, "Long"];


bombTowers <- TowersByDist(bombLat, bombLong, towerLocs);
bomb.distances <- bombTowers$Dist[order(bombTowers$TowerId)][1:1139];

bomb.dursums <- apply(bombXpre, 1, sum);
bomb.best200 <- c(order(bomb.dursums, decreasing=TRUE)[1:199], 337);
bomb.bestDist <- bomb.distances[bomb.best200];

bombXsub <- bombXdur[bomb.best200,bomb.best200];
bombXsub <- bombXsub[order(bomb.bestDist),order(bomb.bestDist)];

ggplot(melt(bombXsub), aes(X1,X2, fill=log(value+1))) +
  geom_raster() +
  scale_fill_gradient2(low="black", mid="red", high="yellow", midpoint=3, guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)







bomb.subset.pre <- bomb.pre.5d$L[bomb.best200,];
bomb.subset.pre <- bomb.subset.pre[order(bomb.bestDist),];

bomb.dist.subset.pre <- DistanceMatrix(bomb.subset.pre);

df.bomb.dist.subset.pre <- melt(bomb.dist.subset.pre);

bomb.subset.dur <- bomb.dur.5d$L[bomb.best200,];
bomb.subset.dur <- bomb.subset.dur[order(bomb.bestDist),];

bomb.dist.subset.dur <- DistanceMatrix(bomb.subset.dur);

df.bomb.dist.subset.dur <- melt(bomb.dist.subset.dur);


df.bomb.dist.subset.diff <- melt(bomb.dist.subset.pre - bomb.dist.subset.dur);

ggplot(df.bomb.dist.subset.pre, aes(X1,X2, fill=value)) + geom_raster()

bombLatentDistChgPlot <- ggplot(df.bomb.dist.subset.diff, aes(X1,X2, fill=-value)) +
  geom_raster() +
  scale_fill_gradient2("", low="red", mid="gray", high="blue", guide=FALSE, limits=c(-20, 20),
                       breaks=c(-20, 0, 20), labels=c("Closer", "Same", "Further")) +
  guides( fill=guide_colorbar(barwidth=2, barheight=20) ) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), text=element_text(size=20)) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");
save(df.bomb.dist.subset.diff, file="df.bomb.dist.subset.diff.rdata");

ggsave("BombingLatentDistanceChange.png", height=5, width=6.5, plot=bombLatentDistChgPlot);



towerOrder <- bomb.best200[order(bomb.bestDist)];

index790 <- (1:200)[towerOrder==790];
index337 <- (1:200)[towerOrder==337];


Tower790BombPlot <- ggplot(df.bomb.dist.subset.diff[df.bomb.dist.subset.diff$X2==index790,],
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

Tower337BombPlot <- ggplot(df.bomb.dist.subset.diff[df.bomb.dist.subset.diff$X2==index337,],
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

ggsave("Tower790BombPlot.png", height=.7, width=5, plot=Tower790BombPlot);
ggsave("Tower337BombPlot.png", height=.7, width=5, plot=Tower337BombPlot);
