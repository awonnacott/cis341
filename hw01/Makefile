DIRS := util,grading
LIBS := unix,str
CFLAGS := -w,+a-4-26-27-32-33-35-39-40-42-45
SUBMIT := hellocaml.ml providedtests.ml

.PHONY: all test clean format main.native main.byte

all: main.native

main.native: $(SUBMIT)
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -libs $(LIBS) main.native

main.byte:
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -libs $(LIBS) main.byte

test: all
	./main.native --test

clean:
	ocamlbuild -clean

format: hellocaml.ml
	ocamlformat -i --ocp-indent-compat -m 100 hellocaml.ml
