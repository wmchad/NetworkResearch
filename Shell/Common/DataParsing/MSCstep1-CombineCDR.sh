# This script combines the call data records for the given
# year and month and puts them in a temporary folder under CDR_chad
# Example call: ./MSCstep1-CombineCDR.sh 2011 05

mkdir -p "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/CombinedCDR"
cd "/ischool/jb/roshan_anon/CDR_ANON/$1-$2/MSC"

for D in `find . -type d`
do
    echo ${D:2}
    find ${D}/*.cdr -exec cat {} > "/ischool/jb/roshan_anon/CDR_chad/$1-$2/MSC/CombinedCDR/MSC-$1-$2-${D:2}.cdr" \;
done
