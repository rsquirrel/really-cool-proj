# Author: Yan Zou
# Usage: test.bash <path>
#        The path is for the compiler.
#        If no path is specified, ../src/TML_Compiler/tmlc is used as default

#!/bin/bash

if [ $# -lt 1 ]
then
	echo "No path for the compiler specified, use ../src/TML_Compiler/tmlc as default" 
	tmlc="../src/TML_Compiler/tmlc"
else
	tmlc=$1
fi

if [ ! -e "$tmlc" ]
then
	echo "TML compiler not found!"
	exit
fi

for file in SrcCode/*.tml
do
	echo "$tmlc < $file"
	$tmlc < $file
	filename=`expr $file : '\(^.*\)'[.]`
	output=$filename.output
	#echo "Compare with $output"
done	

echo "Tests done."
