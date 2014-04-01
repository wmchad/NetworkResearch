# This script combines all the files matching a pattern in a given
# folder and puts the combined files in the given file
# Called as ./CombineFiles.sh path/to/look/in match output/file

cd $1
find $2 -exec cat {} > $3 \;
