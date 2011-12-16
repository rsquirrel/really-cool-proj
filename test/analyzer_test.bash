# Author: Yan Zou
# Usage:
#   1. Run "make scanner_test" in src to generate a scanner_test.
#   2. Run "scanner_test.bash" and pass in the path of scanner_test as the first argument.
#      If no path is specified, "../src/TML_Compiler/scanner_test" is used as default.
#      This script will generate .scan file for each .tml file, recording the result of the scanner
#   3. Run "scanner_test.bash clean" to remove all the .s files.

#!/bin/bash

if [ $# -lt 1 ]
then
	echo "No path for the scanner_test specified, use ../src/TML_Compiler/scanner_test as default" 
	tmlc="../src/TML_Compiler/analyzer_test"
elif [ "$1" = "clean" ]
then
	rm SrcCode/*.sast
	exit
else
	tmlc=$1
fi

if [ ! -e "$tmlc" ]
then
	echo "analyzer_test not found!"
	exit
fi

for file in SrcCode/*.tml
do
	filename=`expr $file : '\(^.*\)'[.]`
	scan_result=$filename.sast
	echo "$tmlc < $file > $scan_result"
	$tmlc < $file > $scan_result
done	

echo "Scanner tests done."
