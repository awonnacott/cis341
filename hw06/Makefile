INCLUDES= util,x86,grading,ll
LIBS = unix,str
PKGS = num
SUBMIT := solver.ml alias.ml backend.ml dce.ml constprop.ml

.PHONY: all test clean format

all: main.native

test: all
	./main.native --test

main.native:
	ocamlbuild -cflags -w,+a-4-40-42 -Is $(INCLUDES) -libs $(LIBS) -pkg $(PKGS) main.native -use-menhir -yaccflag --explain

printanalysis.native:
	ocamlbuild -cflags -w,+a-4-40-42 -Is $(INCLUDES) -libs $(LIBS) -pkg $(PKGS) printanalysis.native -use-menhir -yaccflag --explain

clean:
	ocamlbuild -clean
	rm -rf output a.out

format: solver.ml alias.ml dce.ml constprop.ml backend.ml
	ocamlformat -i --ocp-indent-compat -m 100 solver.ml alias.ml dce.ml constprop.ml backend.ml
