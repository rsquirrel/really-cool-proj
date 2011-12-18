# Author: Yan Zou
# Usage:
#   1. Run "make analyzer_test" in src to generate a scanner_test.
#   2. Run "analyzer_test.bash" and pass in the path of analyzer_test as the first argument.
#      If no path is specified, "../src/TML_Compiler/analyzer_test" is used as default.
#      This script will generate .sast file for each .tml file, recording the result of the scanner
#   3. Run "analyzer_test.bash clean" to remove all the .sast files.

#!/bin/bash

if [ $# -lt 1 ]
then
	echo "No path for the analyzer_test specified, use ../src/TML_Compiler/analyzer_test as default" 
	tmlc="../src/TML_Compiler/analyzer_test"
elif [ "$1" = "clean" ]
then
	rm SastOutput/*.sast
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
	filename=`expr $file : '[^/]*/\(.*\)'`
	scan_result=SastOutput/$filename.sast
	echo "$tmlc < $file > $scan_result"
	$tmlc < $file > $scan_result
done	

echo "Analyzer tests done."
