# This script deletes all the files matching a pattern in a given folder recursively
# Called as ./DeleteFilesRecursive.sh path/to/look/in match

cd $1
rm -r $2
