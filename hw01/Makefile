DIRS := util,grading
LIBS := unix,str
SUBMIT := hellocaml.ml providedtests.ml

.PHONY: all test clean format

all: main.native

main.native: $(SUBMIT)
	ocamlbuild -cflags -w,+a-4-42 -Is $(DIRS) -libs $(LIBS) main.native

main.byte:
	ocamlbuild -cflags -w,+a-4-42 -Is $(DIRS) -libs $(LIBS) main.byte

test: all
	./main.native --test

clean:
	ocamlbuild -clean

format: hellocaml.ml
	ocamlformat -i --ocp-indent-compat -m 100 hellocaml.ml
