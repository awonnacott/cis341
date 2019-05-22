DIRS := util,grading,x86
LIBS := unix,str,nums
SUBMIT := simulator.ml studenttests.ml
OTHER := gradedtests.ml main.ml 

.PHONY: all test clean zip

main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -Is $(DIRS) -libs $(LIBS) main.native

main.byte: $(SUBMIT) $(OTHER)
	ocamlbuild -Is $(DIRS) -libs $(LIBS) main.byte

all: main.byte main.native

test: main.native
	./main.native --test

clean:
	ocamlbuild -clean
