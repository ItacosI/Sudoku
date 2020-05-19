all: sudoku.native

%.native: *.ml
	ocamlc -c sudoku.ml
	ocamlc -c image.ml
	ocamlc -c graph.ml
	ocamlc -o exo graphics.cma sudoku.cmo image.cmo graph.cmo 
	# ocamlc -o exo graphics.cma sudoku.ml image.ml graph.ml
	./exo

.PHONY: clean

clean:
	rm -f *.cmo *.cmi sudoku.cma

