## Script for compiling the number of incoming and outgoing calls
## for a given tower, year and month
## Arguments should be:
##   Tower Id
##   Year (YY)
##   Month (MM)
##   Number of Seconds per period
##   TowerFile (or blank to use default)
## Example call: Rscript TowerCallsPerPeriod.r 11 12 405 300 /ischool/jb/roshan_anon/wmchad/Data/TowerCalls/2011-12/Agg5min/2011-12-Tower405In-5min.txt /ischool/jb/roshan_anon/wmchad/Data/TowerCalls/2011-12/Agg5min/2011-12-Tower405Out-5min.txt

args <- commandArgs(trailingOnly = TRUE);
year <- as.numeric(args[1]);
month <- as.numeric(args[2]);
towerId <- as.numeric(args[3]);
periodLength <- as.numeric(args[4]);
outputInFile <- args[5];
outputOutFile <- args[6];

setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Functions");
source("Data/TowerFunctions.r");

## Get the call files
## format is (Year, Month, Day, Hour, Minute, Second, FromTowerId, ToTowerId, Duration)
inCalls <- GetTowerInCalls(towerId, year, month);
outCalls <- GetTowerOutCalls(towerId, year, month);

## Combine the data and calculate averages (according to period and day of the week)
inCalls.agg <- AggregateCalls(inCalls, periodLength);
outCalls.agg <- AggregateCalls(outCalls, periodLength);

inCalls.agg$AvgCalls <- CalculateWeeklyAverages(inCalls.agg);
outCalls.agg$AvgCalls <- CalculateWeeklyAverages(outCalls.agg);

## Save combined files
write.table( inCalls.agg, outputInFile, quote=FALSE,
             row.names=FALSE, col.names=FALSE, sep="|" );
write.table( outCalls.agg, outputOutFile, quote=FALSE,
             row.names=FALSE, col.names=FALSE, sep="|" );
