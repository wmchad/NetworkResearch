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



## inauguration

inaug2Xpre <- CombineSnapshots( 11, 7, 23, 12, 30, 5, 6 );
inaug2Xdur <- CombineSnapshots( 11, 7, 23, 15, 0, 5, 6 );

ldim <- 5;
lambda <- 100;
maxiter <- 500;
epsilon <- 1e-06;

inaug2.pre.5d <- ALS(inaug2Xpre, ldim, lambda,
                    NULL, NULL, maxiter, epsilon);
inaug2.dur.5d <- ALS(inaug2Xdur, ldim, lambda,
                    NULL, NULL, maxiter, epsilon);
print("finished");



towerLocs <- TowerLocations();

inaug2Lat <- towerLocs[261, "Lat"];
inaug2Long <- towerLocs[261, "Long"];


inaug2Towers <- TowersByDist(inaug2Lat, inaug2Long, towerLocs);
inaug2.distances <- inaug2Towers$Dist[order(inaug2Towers$TowerId)][1:1139];

inaug2.dursums <- apply(inaug2Xpre, 1, sum);
inaug2.best200 <- c(order(inaug2.dursums, decreasing=TRUE)[1:198], 261, 92);
inaug2.bestDist <- inaug2.distances[inaug2.best200];

inaug2Xsub <- inaug2Xdur[inaug2.best200,inaug2.best200];
inaug2Xsub <- inaug2Xsub[order(inaug2.bestDist),order(inaug2.bestDist)];

ggplot(melt(inaug2Xsub), aes(X1,X2, fill=log(value+1))) +
  geom_raster() +
  scale_fill_gradient2(low="black", mid="red", high="yellow", midpoint=3, guide=FALSE) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)







inaug2.subset.pre <- inaug2.pre.5d$L[inaug2.best200,];
inaug2.subset.pre <- inaug2.subset.pre[order(inaug2.bestDist),];

inaug2.dist.subset.pre <- DistanceMatrix(inaug2.subset.pre);

df.inaug2.dist.subset.pre <- melt(inaug2.dist.subset.pre);

inaug2.subset.dur <- inaug2.dur.5d$L[inaug2.best200,];
inaug2.subset.dur <- inaug2.subset.dur[order(inaug2.bestDist),];

inaug2.dist.subset.dur <- DistanceMatrix(inaug2.subset.dur);

df.inaug2.dist.subset.dur <- melt(inaug2.dist.subset.dur);


df.inaug2.dist.subset.diff <- melt(inaug2.dist.subset.pre - inaug2.dist.subset.dur);

ggplot(df.inaug2.dist.subset.pre, aes(X1,X2, fill=value)) + geom_raster()

inaug2LatentDistChgPlot <- ggplot(df.inaug2.dist.subset.diff, aes(X1,X2, fill=-value)) +
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
save(df.inaug2.dist.subset.diff, file="df.inaug2.dist.subset.diff.rdata");

ggsave("Inauguration2LatentDistanceChange.png", height=5, width=6.5, plot=inaug2LatentDistChgPlot);



towerOrder <- inaug2.best200[order(inaug2.bestDist)];

index261 <- (1:200)[towerOrder==261];
index92 <- (1:200)[towerOrder==92];


Tower261InaugPlot <- ggplot(df.inaug2.dist.subset.diff[df.inaug2.dist.subset.diff$X2==index261,],
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

Tower92InaugPlot <- ggplot(df.inaug2.dist.subset.diff[df.inaug2.dist.subset.diff$X2==index92,],
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

ggsave("Tower261InaugPlot.png", height=.7, width=5, plot=Tower261InaugPlot);
ggsave("Tower92InaugPlot.png", height=.7, width=5, plot=Tower92InaugPlot);
