# Author: Yan Zou, Jiabin Hu
# Usage: test.bash <srcfile>
#        srcfile: the source file to be tested.
#        If no srcfile is specified, it will run all test cases.

#!/bin/bash

tmlc="../src/TML_Compiler/tmlc"

if [ $# -lt 1 ]
then
	echo "Running all tests..." 
else
	file=$1
    echo "Testing $file"
    $tmlc $file
    filename=`expr $file : '\(^.*\)'[.]`
    java -classpath ../src/TML_Interpreter Main $filename.tmb > $filename.result
    output=$filename.output
    diff $filename.result $output
	exit 0
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
	java -classpath ../src/TML_Interpreter Main $filename.tmb > $filename.result
	output=$filename.output
	diff $filename.result $output
	#echo "Compare with $output"
done

#echo ""
#echo "ByteCode tests"
#for file in Bytecode/*.bc
#do
#	echo "Testing $file"
#	filename=`expr $file : '\(^.*\)'[.]`
#	java -classpath ../src/TML_Interpreter Main $file > $filename.result
#	stdput=$filename.output
#	diff $filename.result $stdput
#	#echo "Compare with $output"
#done	

echo "Tests done."
