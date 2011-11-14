tmlc:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	#ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c compiler.ml
	ocamlc -o tmlc scanner.cmo parser.cmo compiler.cmo

scanner_test:
	ocamlc -o scanner_test scanner.cmo parser.cmo scanner_test.ml

clean:
	rm *.cmi *.cmo scanner.ml parser.mli parser.ml tmlc scanner_test

