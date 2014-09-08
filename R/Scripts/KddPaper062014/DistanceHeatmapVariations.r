TowerDistanceDifferencesPlot <- function(towerId, towerLocs, subLocs,
                                         dmfResult, eventIndex, xAxis,
                                         xLab="xxx", yLab="yyy",
                                         plotTitle="Title") {
  towerLat <- towerLocs$Lat[towerId];
  towerLong <- towerLocs$Long[towerId];

  eventTowers <- TowersByDist(towerLat, towerLong, subLocs)[-1,];

  subL <- dmfResult$L[eventTowers$TowerId,,];

  towerL <- dmfResult$L[towerId,,];

  T <- ncol(towerL);

  towerDistances <- matrix(data=0, nrow=nrow(eventTowers), ncol=T);

  for ( i in 1:nrow(eventTowers) ) {
    towerDistances[i,1] <- sqrt(sum((towerL[,1] - subL[i,,1])^2));
  }
  for (t in 2:T) {
    for ( i in 1:nrow(eventTowers) ) {
      towerDistances[i,t] <- sqrt(sum((towerL[,t] - subL[i,,t])^2));
      towerDistances[i,t-1] <- towerDistances[i,t] - towerDistances[i,t-1];
    }
  }
  towerDistances <- towerDistances[,-T];

  dfDist <- melt(towerDistances);
  colnames(dfDist) <- c("Tower", "Time", "DistDiff");
  dfDist$Tower <- dfDist$Tower;
#  print(paste("Max Distance:", max(dfDist$DistDiff)));
#  print(paste("Min Distance:", min(dfDist$DistDiff)));
  
  ggplot(dfDist, aes(Time, Tower, fill=DistDiff)) + geom_raster() +
    scale_fill_gradient2(low="red", mid="grey", high="blue",
                         midpoint=0, guide=FALSE, limits=c(-5, 5)) + xAxis +
      annotate("point", x=eventIndex, y=-14, color="green", size=7) +
        theme(text=element_text(size=20), axis.ticks.y=element_blank(),
              axis.text.y=element_blank()) +
          labs(x=xLab, y=yLab, title=plotTitle);
}

TowerRelativePositionsPlots <- function(towerId, towerLocs, subLocs,
                                        dmfResult, eventIndex,
                                        dim1=1, dim2=2) {
  towerLat <- towerLocs$Lat[towerId];
  towerLong <- towerLocs$Long[towerId];

  eventTowers <- TowersByDist(towerLat, towerLong, subLocs)[-1,];

  subL <- dmfResult$L[eventTowers$TowerId,c(dim1, dim2),];

  towerL <- dmfResult$L[towerId,c(dim1, dim2),];

  T <- ncol(towerL);

  xrng <- yrng <- c(0,0);
  locDf <- NULL;

  for ( t in 1:T ) {
    relL <-t(apply(subL[,,t], 1, function(lpos) {lpos - towerL[,t];}));
    locDf[[t]] <- data.frame(X1=relL[,1], X2=relL[,2], Dist=eventTowers$Dist);
    xrng <- range(c(xrng, relL[,1]));
    yrng <- range(c(yrng, relL[,2]));
  }
  plotlist <- NULL;
  for ( t in 1:T ) {
    plotlist[[t]] <- ggplot( locDf[[t]], aes(X1, X2, color=Dist) ) +
      geom_point() + xlim(xrng) + ylim(yrng);
  }

  plotlist;
}

TowerRelativeMotionsPlots <- function(towerId, towerLocs, subLocs,
                                      dmfResult, eventIndex,
                                      dim1=1, dim2=2) {
  towerLat <- towerLocs$Lat[towerId];
  towerLong <- towerLocs$Long[towerId];

  eventTowers <- TowersByDist(towerLat, towerLong, subLocs)[-1,];

  subL <- dmfResult$L[eventTowers$TowerId,c(dim1, dim2),];

  towerL <- dmfResult$L[towerId,c(dim1, dim2),];

  T <- ncol(towerL);

  xrng <- yrng <- c(0,0);
  locDf <- NULL;

  for ( t in 1:T ) {
    relL <-t(apply(subL[,,t], 1, function(lpos) {lpos - towerL[,t];}));
    locDf[[t]] <- data.frame(X1=relL[,1], X2=relL[,2], Dist=eventTowers$Dist);
    xrng <- range(c(xrng, relL[,1]));
    yrng <- range(c(yrng, relL[,2]));
  }
  plotlist <- NULL;
  for ( t in 2:T ) {
    tdf <- data.frame(X1start=locDf[[t-1]]$X1, X1end=locDf[[t]]$X1,
                      X2start=locDf[[t-1]]$X2, X2end=locDf[[t]]$X2,
                      Dist=locDf[[t]]$Dist);
    plotlist[[t-1]] <- ggplot( tdf, aes(x=X1start, xend=X1end,
                                      y=X2start, yend=X2end, color=Dist) ) +
                                        geom_segment(arrow=arrow(length=unit(0.5, "cm"))) +
                                          xlim(xrng) + ylim(yrng);
  }

  plotlist;
}



TowerDistanceDifferencesPlot(790, towerLocs, subLocs, bombTMF, 22, TimeAxis(),
                   xLab="", yLab="", plotTitle="Closest Tower");
TowerDistanceDifferencesPlot(405, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 2nd closest
TowerDistanceDifferencesPlot(683, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 10 km
TowerDistanceDifferencesPlot(11, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 102 km
TowerDistanceDifferencesPlot(337, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 166 km
TowerDistanceDifferencesPlot(640, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 208 km


TowerDistanceDifferencesPlot(261, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # closest
TowerDistanceDifferencesPlot(125, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # 2nd closest (14 km)
TowerDistanceDifferencesPlot(874, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); #  124 km
TowerDistanceDifferencesPlot(92, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # 169 km
TowerDistanceDifferencesPlot(790, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # 137 km



plots790 <- TowerRelativePositionsPlots(790, towerLocs, subLocs, bombTMF, 22, 1, 2);

pdf( "testRelativePositions790.pdf", height=8, width=8 );
for ( t in 1:48 ) {
  print(plots790[[t]]);
}
dev.off();

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativePositionsPlots(790, towerLocs, subLocs, bombTMF, 22, x1, x2);
    pdf( paste("testRelativePositions790x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rpPlot in plotlist ) {
      print(rpPlot);
    }
    dev.off();
  }
}

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativePositionsPlots(337, towerLocs, subLocs, bombTMF, 22, x1, x2);
    pdf( paste("testRelativePositions337x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rpPlot in plotlist ) {
      print(rpPlot);
    }
    dev.off();
  }
}

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativePositionsPlots(261, towerLocs, subLocs3, inaugTMF, 31, x1, x2);
    pdf( paste("testRelativePositions261x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rpPlot in plotlist ) {
      print(rpPlot);
    }
    dev.off();
  }
}

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativePositionsPlots(92, towerLocs, subLocs3, inaugTMF, 31, x1, x2);
    pdf( paste("testRelativePositions92x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rpPlot in plotlist ) {
      print(rpPlot);
    }
    dev.off();
  }
}

motions790 <- TowerRelativeMotionsPlots(790, towerLocs, subLocs, bombTMF, 22, 1, 2);

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativeMotionsPlots(790, towerLocs, subLocs, bombTMF, 22, x1, x2);
    pdf( paste("testRelativeMotions790x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rmPlot in plotlist ) {
      print(rmPlot);
    }
    dev.off();
  }
}

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativeMotionsPlots(337, towerLocs, subLocs, bombTMF, 22, x1, x2);
    pdf( paste("testRelativeMotions337x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rmPlot in plotlist ) {
      print(rmPlot);
    }
    dev.off();
  }
}

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativeMotionsPlots(261, towerLocs, subLocs3, inaugTMF, 31, x1, x2);
    pdf( paste("testRelativeMotions261x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rmPlot in plotlist ) {
      print(rmPlot);
    }
    dev.off();
  }
}

setwd("C:/Plots/NetworkResearch/Temp");
for ( x1 in 1:4 ) {
  for ( x2 in (x1+1):5 ) {
    plotlist <- TowerRelativeMotionsPlots(92, towerLocs, subLocs3, inaugTMF, 31, x1, x2);
    pdf( paste("testRelativeMotions92x", x1, "x", x2, ".pdf", sep=""), height=8, width=8 );
    for ( rmPlot in plotlist ) {
      print(rmPlot);
    }
    dev.off();
  }
}
