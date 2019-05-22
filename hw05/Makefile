INCLUDES= util,x86,grading,ll
PKGS = num
LIBS = unix,str
SUBMIT := frontend.ml typechecker.ml

.PHONY: all test clean format

all: main.native

test: all
	./main.native --test

main.native:
	ocamlbuild -cflags -w,+a-4-40-42 -Is $(INCLUDES) -pkg $(PKGS) -libs $(LIBS) main.native -use-menhir -yaccflag --explain

clean:
	ocamlbuild -clean
	rm -rf output a.out

format: frontend.ml typechecker.ml
	ocamlformat -i --ocp-indent-compat -m 100 frontend.ml typechecker.ml
