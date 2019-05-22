DIRS= util,x86,grading,ll
PKGS = num
LIBS = unix,str
SUBMIT := lexer.mll parser.mly frontend.ml studenttests.ml

.PHONY: all test clean format

all: main.native

test: all
	./main.native --test --clang

main.native:
	ocamlbuild -cflags -w,+a-4-40-42 -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) main.native -use-menhir -yaccflag --explain

clean:
	ocamlbuild -clean
	rm -rf output a.out

format: frontend.ml
	ocamlformat -i --ocp-indent-compat -m 100 frontend.ml
