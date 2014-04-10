## This script pulls out the data from the nearest towers
## to a given location for a given month and year
## Example call: ./GetNearTowerEventData.sh <YY> <MM> <lat> <long> <numTowers>

cat $1 | while read yr mo lat long;
do

    ## Make sure the parsed data exists for the month
    if [ ! -f /ischool/jb/roshan_anon/CDR_chad/paired/20$yr-$mo-Paired-Calls.txt ]; then
	cd /ischool/jb/roshan_anon/wmchad/Code/Shell/Common/DataParsing
	./PairedData-Convert.sh 20$yr $mo
    fi
    
    ## Get the list of towers 
    cd /ischool/jb/roshan_anon/wmchad/Code/R/Scripts/Tower
    Rscript NearestTowersToLocation.r $lat $long $2 /ischool/jb/roshan_anon/wmchad/Temp/NearTowers$2-$lat-$long.txt
    
    ## Get the call data for the towers
    cd /ischool/jb/roshan_anon/wmchad/Temp
    cat NearTowers$2-$lat-$long.txt | while read towerId;
    do
	if [ ! -f /ischool/jb/roshan_anon/wmchad/Data/TowerCalls/20$yr-$mo/20$yr-$mo-Tower${towerId}In.txt ]; then
	    cd /ischool/jb/roshan_anon/wmchad/Code/Shell/Common/DataParsing
	    ./PairedData-GetTowerCalls.sh $yr $mo $towerId
	fi
    done
    
done

