# This script calls a scala file to pull out a network
# for a given year, month, day and start and end times
# The output file will have the following fields, pipe-delimited:
#    From tower id
#    To tower id
#    Number of calls
# It then combines the resulting files into a file
# under wmchad/Data/NetworkSnapshots
# Example call: ./PairedData-GetNetwork.sh 11 11 05 12 00 00 12 04 59

mkdir -p "/ischool/jb/roshan_anon/wmchad/Data/NetworkSnapshots/20$1-$2/$3"

scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/Paired/PairedParser.scala GetNetwork /ischool/jb/roshan_anon/CDR_chad/paired/20$1-$2-Paired-Calls.txt $1 $2 $3 $4 $5 $6 $7 $8 $9 /ischool/jb/roshan_anon/wmchad/Data/NetworkSnapshots/20$1-$2/$3/20$1-$2-$3-$4$5$6-$7$8$9-Network

cd "/ischool/jb/roshan_anon/wmchad/Data/NetworkSnapshots/20$1-$2/$3/20$1-$2-$3-$4$5$6-$7$8$9-Network"

find part* -exec cat {} > ../20$1-$2-$3-$4$5$6-$7$8$9-Network.txt \;
rm part*
rm .part*
cd ..
rm -r 20$1-$2-$3-$4$5$6-$7$8$9-Network
