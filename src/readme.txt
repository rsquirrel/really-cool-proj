##
## Tree Manipulating Language (TML)
##
## By Jiabin Hu,     Akash Sharma, 
##    Shuai Sun, and Yan Zou
##
## Columbia University, Dec-22-2011 
##
##

Building, Compiling and Executing Instructions:

0. Prerequisites
  TML project is platform independent. You can use Windows or MacOS or
  Linux. However, we only provide bash scripts for auto build.
  OCaml and Java environment should have benn set up.
  To get OCaml, please refer to the OCaml download page. 
  (http://caml.inria.fr/download.en.html)
  To get Java, please refer to the Java download page.
  (http://java.com)

1. Build 
  To build the compiler and interpreter, cd into 'src' directory and 
  type 'make'. Two files 'tmlc' and 'tml.jar' will appear in the same 
  directory.

2. Compile
  To compile an TML source file, type 'tmlc xxx.tml'. A file named 
  'xxx.tmb' will appear in the same directory of the .tml file.

3. Run
  To execute the .tmb file, type 'tml xxx.tmb'.
