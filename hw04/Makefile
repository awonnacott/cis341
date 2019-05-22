DIRS= util,x86,grading,ll
PKGS = num
LIBS = unix,str
CFLAGS := -w,+a-4-26-27-32-33-35-39-40-42-45
SUBMIT := lexer.mll parser.mly frontend.ml studenttests.ml

.PHONY: all test clean format

all: main.native

test: all
	./main.native --test --clang

main.native:
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) main.native -use-menhir -yaccflag --explain

clean:
	ocamlbuild -clean
	rm -rf output a.out

format: frontend.ml
	ocamlformat -i --ocp-indent-compat -m 100 frontend.ml
