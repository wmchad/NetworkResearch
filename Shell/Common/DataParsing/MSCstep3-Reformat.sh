# This script calls a scala file to reformat the parsed MSC CDR data
# for a given year, month and day to pull out the fields we want
# It then combines the resulting files into the Reformatted directory
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
# Example call: ./MSCstep3-Reformat.sh 2011 11 01

scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/MSC/MSCParser.scala ReformatParsedData /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/ParsedCDR/MSC-Call-$1-$2-$3.txt /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Reformatted/MSC-Call-$1-$2-$3

cd "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Reformatted/MSC-Call-$1-$2-$3"
find part* -exec cat {} > ../MSC-Call-$1-$2-$3.txt \;
rm part*
rm .part*
cd ..
rm -r MSC-Call-$1-$2-$3

rm "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/ParsedCDR/MSC-Call-$1-$2-$3.txt"
