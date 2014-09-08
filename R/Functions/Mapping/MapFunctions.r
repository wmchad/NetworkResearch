## File that contains functions related to mapping

## calculates the distance between two points in kilometers
kmDist <- function( lat1, long1, lat2, long2 ) {
  R <- 6371;
  dLat <- (lat2 - lat1)*pi/180;
  dLong <- (long2 - long1)*pi/180;
  lat1 <- lat1*pi/180;
  lat2 <- lat2*pi/180;
  a <- sin(dLat/2)^2 + sin(dLong/2)^2 * cos(lat1) * cos(lat2);
  2 * R * asin(sqrt(a));
}

## Set up the Afghanistan map for plotting
GetAfghanistanMapData <- function(mapDir="C:/Data/NetworkResearch/AFG_adm") {
  currdir <- getwd();
  setwd(mapDir);
  afg <- readOGR(dsn=".", layer="AFG_adm1");
  setwd(currdir);
  afg;
}
