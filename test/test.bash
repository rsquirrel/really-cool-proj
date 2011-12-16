# Author: Yan Zou
# Usage: test.bash <path>
#        The path is for the compiler.
#        If no path is specified, ../src/TML_Compiler/tmlc is used as default

#!/bin/bash

if [ $# -lt 2 ]
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

echo "SrcCode tests"
for file in SrcCode/*.tml
do
	echo "Testing $file"
	$tmlc $file
	filename=`expr $file : '\(^.*\)'[.]`
	output=$filename.output
	#echo "Compare with $output"
done

echo ""
echo "ByteCode tests"
for file in Bytecode/*.bc
do
	echo "Testing $file"
	filename=`expr $file : '\(^.*\)'[.]`
	java -classpath ../src/TML_Interpreter Main $file > $filename.result
	stdput=$filename.output
	diff $filename.result $stdput
	#echo "Compare with $output"
done	

echo "Tests done."
