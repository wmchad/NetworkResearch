setwd("c:/Code/NetworkResearch/R/Test/CPMF/Results");

load("bombingCpmf.rdata");

dfBombResults <- data.frame(beta=0, ldim=0, cps="", stringsAsFactors=FALSE);
for ( i in 1:length(cpResult) ) {
  res <- cpResult[[i]];
  dfBombResults[i,] <- c(res$beta, res$ldim,
                         paste(res$result$cp, collapse=","));
}

cpCounts <- rep(0, 9);

for ( i in 1:length(cpResult) ) {
  res <- cpResult[[i]];
  for ( cp in res$result$cp ) {
    cpCounts[cp] <- cpCounts[cp] + 1;
  }
}



for ( b in c("100", "1000", "1e4", "1e5", "1e6", "1e7", "1e8") ) {
  load(paste("cpResult.inaug.b", b, ".rdata", sep=""));
}


dfInaugResults <- data.frame(beta=0, ldim=0, cps="", stringsAsFactors=FALSE);
cpCounts.inaug <- rep(0, 23);
index <- 1;
for ( i in 1:length(cpResult.inaug.b100) ) {
  res <- cpResult.inaug.b100[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
for ( i in 1:length(cpResult.inaug.b1000) ) {
  res <- cpResult.inaug.b1000[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
for ( i in 1:length(cpResult.inaug.b1e4) ) {
  res <- cpResult.inaug.b1e4[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
for ( i in 1:length(cpResult.inaug.b1e5) ) {
  res <- cpResult.inaug.b1e5[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
for ( i in 1:length(cpResult.inaug.b1e6) ) {
  res <- cpResult.inaug.b1e6[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
for ( i in 1:length(cpResult.inaug.b1e7) ) {
  res <- cpResult.inaug.b1e7[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
for ( i in 1:length(cpResult.inaug.b1e8) ) {
  res <- cpResult.inaug.b1e8[[i]];
  dfInaugResults[index,] <- c(res$beta, res$ldim,
                              paste(res$result$cp, collapse=","));
  for ( cp in res$result$cp ) {
    cpCounts.inaug[cp] <- cpCounts.inaug[cp] + 1;
  }
  index <- index + 1;
}
