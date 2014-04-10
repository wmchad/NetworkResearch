# This script calls a scala file to combine the reformatted MSC data
# for a given year, month and day to match incoming and outgoing calls
# It then combines the resulting files into the Matched directory
# under CDR_chad
# The final file has the following fields, semicolon delimited:
#   From caller id
#   To Caller id
#   From Tower id start (Tower where the caller started)
#   From Tower id end (Tower where the caller ended)
#   To Tower id start (Tower where the receiver started)
#   To Tower id end (Tower where the receiver ended)
#   Year (YYYY)
#   Month (MM)
#   Day (DD)
#   Hour (HH)
#   Minute (MM)
#   Second (SS)
#   Duration of call in seconds
# Example call: ./MSCstep4-Match.sh 2011 11 01

scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/MSC/MSCParser.scala MatchCallRecords /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Reformatted/MSC-Call-$1-$2-$3.txt /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Matched/MSC-Call-$1-$2-$3

cd "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Matched/MSC-Call-$1-$2-$3"
find part* -exec cat {} > ../MSC-Call-$1-$2-$3.txt \;
rm part*
rm .part*
cd ..
rm -r MSC-Call-$1-$2-$3

rm "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Reformatted/MSC-Call-$1-$2-$3.txt"
