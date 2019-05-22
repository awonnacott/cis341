all:

format:
	ocamlformat -i --ocp-indent-compat -m 100 **/*.ml
