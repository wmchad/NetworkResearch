setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Mapping/MapFunctions.r");

towers <- TowerLocations();


## Closest towers to bombing on 12/6/2011
TowersByDist( 34.51828384, 69.18315125, towers )[1:5, c(1,5)];
##     TowerId      Dist
## 790     790 0.1273375
## 405     405 0.2131581
## 334     334 0.4241063
## 204     204 0.4468841
## 127     127 0.4804276

## Closest towers to Karzai stadium in Lashkar Gah
TowersByDist( 31.583712, 64.358153, towers )[1:5, c(1,5)];
##      TowerId      Dist
## 636      636 0.2234602
## 834      834 0.6495569
## 91        91 0.6926836
## 928      928 0.8555202
## 1091    1091 0.8559428

## Closest towers to Kabul Nation Cricket Stadium
TowersByDist( 34.515224, 69.198095, towers )[1:5, c(1,5)];
##     TowerId      Dist
## 183     183 0.5056664
## 283     283 0.6774467
## 634     634 0.6820358
## 364     364 1.0246661
## 840     840 1.1448590

## Closest towers to Ghazi stadium in Lashkar Gah
TowersByDist( 34.518573, 69.193509, towers )[1:5, c(1,5)];
##     TowerId      Dist
## 183     183 0.4277249
## 840     840 0.6033923
## 634     634 0.7034785
## 944     944 0.8699739
## 790     790 0.9206001


790, 405, 636, 834, 183, 283, 840
