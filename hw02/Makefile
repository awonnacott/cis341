DIRS := util,grading,x86
PKGS := num
LIBS := unix,str
SUBMIT := simulator.ml studenttests.ml

.PHONY: all test clean format

all: main.native

main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags -w,+a-4-42 -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) main.native

main.byte: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags -w,+a-4-42 -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) main.byte

test: all
	./main.native --test

clean:
	ocamlbuild -clean

format: simulator.ml
	ocamlformat -i --ocp-indent-compat -m 100 simulator.ml
