require(ggplot2);
require(reshape2);

setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");
source("Plotting/PlotUtilities.r");

setwd("C:/Data/NetworkResearch/TowerCalls/2011-12/Agg5min");

allCalls <- read.table("2011-12-AllCalls.txt", sep="|");
colnames(allCalls) <- c("Day", "Period", "Calls");

allCalls <- allCalls[order(allCalls$Day, allCalls$Period),];
allCalls$MonthPeriod <- (allCalls$Day - 1) * 288 + allCalls$Period;

ggplot(allCalls, aes(MonthPeriod, Calls)) +
  geom_line()

towerCalls <- GetTowerAggregatedCalls( 405, 11, 12 );

towerCalls$MonthNormVol <- towerCalls$TotalCalls / allCalls$Calls;

ggplot(towerCalls[towerCalls$Day==6,], aes(Period, MonthNormVol)) +
  geom_line()

subCalls <- towerCalls[towerCalls$DayPeriod >= 12*6 & towerCalls$DayPeriod <= 12*18,];
subCalls$AdjPeriod <- 1:nrow(subCalls);

ggplot(subCalls, aes(AdjPeriod, MonthNormVol)) +
  geom_line()

towerCalls$InOutDiffCalls <- towerCalls$InCalls - towerCalls$OutCalls;

ggplot(towerCalls, aes(Period, InOutDiffCalls)) +
  geom_line()

towerInCalls <- GetTowerInCalls( 405, 11, 12 );

towerInCalls$MonthSecond <- towerInCalls$Second +
  towerInCalls$Minute*60 + towerInCalls$Hour*60*60 +
  (towerInCalls$Day-1)*60*60*24;

towerInCalls <- towerInCalls[order(towerInCalls$MonthSecond),];

## 5-minute windows

startSeconds <- seq(from=0, to=(60*60*24 - 60), 60);
endSeconds <- startSeconds + 60*5 - 1;

windowVolume <- sapply(1:(60*24), function(i) {
  sum(towerInCalls$MonthSecond >= startSeconds[i] &
      towerInCalls$MonthSecond <= endSeconds[i]);
});

windowVolume6 <- sapply(1:(60*24), function(i) {
  sum(towerInCalls$MonthSecond >= (24*60*60*5 + startSeconds[i]) &
      towerInCalls$MonthSecond <= (24*60*60*5 + endSeconds[i]));
});
