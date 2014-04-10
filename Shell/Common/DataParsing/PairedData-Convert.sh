# This script calls a scala file to convert paired data for a given year and month
# to the format I would like to
# work with. The input paired file should have the following fields, pipe-delimited:
#    From Caller
#    To Caller
#    Date (YYMMDD)
#    Time (HH:MM:SS)
#    Duration in seconds
#    From tower id
#    From location access code (LAC)
#    To tower id
#    To location access code (LAC)
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
# Here, the combined tower ids use a single identifier for all tower ids
# in the same geolocation
# It then combines the resulting files into the paired directory
# under CDR_chad
# Example call: ./PairedData-Convert.sh 2011 11

cd "/ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/Paired"
java -jar PairedParser.jar ConvertFile  /ischool/jb/roshan_anon/parsed/$1-$2/$1-$2-Paired-Call.txt /ischool/jb/roshan_anon/CDR_chad/towerMap-2012-10.txt /ischool/jb/roshan_anon/CDR_chad/paired/$1-$2-Paired 

cd "/ischool/jb/roshan_anon/CDR_chad/paired/$1-$2-Paired"

find part* -exec cat {} > ../$1-$2-Paired-Calls.txt \;
rm part*
rm .part*
cd ..
rm -r $1-$2-Paired
