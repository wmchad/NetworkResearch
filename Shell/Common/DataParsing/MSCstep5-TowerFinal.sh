# This script calls a scala file to pull out just the fields needed
# for analyzing data at the tower level
# It then combines the resulting files into the TowerFinal directory
# under CDR_chad
# The final file has the following fields, semicolon delimited:
#   Year (YYYY)
#   Month (MM)
#   Day (DD)
#   Hour (HH)
#   Minute (MM)
#   Second (SS)
#   From tower id
#   To tower id
#   Duration of call in seconds
# Example call: ./MSCstep5-TowerFinal.sh 2011 11 01

scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/MSC/MSCParser.scala FinalFormatTower /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Matched/MSC-Call-$1-$2-$3.txt /ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/TowerFinal/MSC-Call-$1-$2-$3 /ischool/jb/roshan_anon/gis/TowerTranslation

cd "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/TowerFinal/MSC-Call-$1-$2-$3"
find part* -exec cat {} > ../MSC-Call-$1-$2-$3.txt \;
rm part*
rm .part*
cd ..
rm -r MSC-Call-$1-$2-$3

# rm "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/Reformatted/MSC-Call-$1-$2-$3.txt"
