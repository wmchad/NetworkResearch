# This script calls a scala file to parse the MSC CDR data
# for a given year, month and day to pull out the fields we want
# It then combines the resulting files into the ParsedCDR directory
# under CDR_chad
# The final file has the following fields, semicolon delimited:
#   Record Type
#   Served IMSI (caller for moCallRecord (0), moSMSRecord (6);
#                receiver for mtCallRecord(1), mtSMSRecord (7))
#   Served mobile station ISDN number
#   Non-served mobile station ISDN number (opposite of served)
#   First Tower Id for served party (where they were when the call started)
#   Last Tower Id for served party (where they were when the call ended)
#   Day (YYYYMMDD format)
#   Time (HHMMSS format)
#   Duration of call in seconds
# Example call: ./MSCstep2-ParseCDR.sh 2011 11 01

scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/MSC/MSCParser.scala /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/CombinedCDR/MSC-$1-$2-$3.cdr /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/ParsedCDR/MSC-Call-$1-$2-$3 /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/ParsedCDR/MSC-SMS-$1-$2-$3

cd "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/ParsedCDR/MSC-Call-$1-$2-$3"
find part* -exec cat {} > ../MSC-Call-$1-$2-$3.txt \;
rm part*
rm .part*
cd ..
rm -r MSC-Call-$1-$2-$3

cd "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/ParsedCDR/MSC-SMS-$1-$2-$3"
find part* -exec cat {} > ../MSC-SMS-$1-$2-$3.txt \;
rm part*
rm .part*
cd ..
rm -r MSC-SMS-$1-$2-$3

rm "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/CombinedCDR/MSC-$1-$2-$3.cdr"

