# This script calls a scala file to pull out all the calls
# from and to a given tower for a given year and month
# The output file will have the following fields, pipe-delimited:
#    Year
#    Month
#    Day
#    Hour
#    Minute
#    Second
#    From tower id (combined)
#    To tower id (combined)
#    Duration in seconds
# It then combines the resulting files into a file
# under wmchad/Data/TowerCalls/YYYY-MM
# Example call: ./PairedData-GetTowerCalls.sh 11 11 927

mkdir -p "/ischool/jb/roshan_anon/wmchad/Data/TowerCalls/20$1-$2"

cd "/ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/Paired"
java -jar PairedParser.jar GetTowerCalls /ischool/jb/roshan_anon/CDR_chad/paired/20$1-$2-Paired-Calls.txt $3 /ischool/jb/roshan_anon/wmchad/Data/TowerCalls/20$1-$2/20$1-$2-Tower$3In /ischool/jb/roshan_anon/wmchad/Data/TowerCalls/20$1-$2/20$1-$2-Tower$3Out

cd "/ischool/jb/roshan_anon/wmchad/Data/TowerCalls/20$1-$2"

find 20$1-$2-Tower$3In/part* -exec cat {} > 20$1-$2-Tower$3In.txt \;
find 20$1-$2-Tower$3Out/part* -exec cat {} > 20$1-$2-Tower$3Out.txt \;
rm -r 20$1-$2-Tower$3In
rm -r 20$1-$2-Tower$3Out
