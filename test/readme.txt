##
## Tree Manipulating Language (TML)
##
## By Jiabin Hu,     Akash Sharma, 
##    Shuai Sun, and Yan Zou
##
## Columbia University, Dec-22-2011 
##
##

Test Suite Instructions:

0. Prerequisite
  Before running test cases, the TML compiler and TML interpreter should 
  have been built. Check if you got 'tmlc' and 'tml' under the src directory.
  If not, please build them with instructions in the readme file under src
  directory.

1. Test compiler:
  First of all, you need to build the compiler (cd into 'src' and type 'make')
  To run all cases, type './test.bash'
  To run a single test case, type './test.bash SrcCode/xxx.tml', 
    where 'xxx.tml' is the name of the test case.

2. Test scanner:
  1) Use Makefile under src directory to generate a scanner_test.
     Run "make scanner_test" under src directory.
  2) Run "scanner_test.bash" and pass in the path of executable scanner_test 
     as the first argument.
     If no path is specified, "../src/scanner_test" is used as default.
     This script will generate .scan file for each .tml file, recording the 
     result of the scanner
  3) Run "scanner_test.bash clean" to remove all the .scan files.

3. Test analyzer:
  1) Use Makefile under src directory to generate a analyzer_test.
     Run "make analyzer_test" under src directory.
  2) Run "analyzer_test.bash" and pass in the path of executable analyzer_test
     as the first argument.
     If no path is specified, "../src/analyzer_test" is used as default.
     This script will generate .sast file in directory SastOutput for each 
     .tml file, recording the result of the analyzer.
  3) Run "analyzer_test.bash clean" to remove all the .sast files.
