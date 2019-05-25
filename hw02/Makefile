DIRS := util,grading,x86
PKGS := num
LIBS := unix,str
CFLAGS := -w,+a-4-26-27-32-33-35-39-40-42-45
SUBMIT := simulator.ml studenttests.ml

.PHONY: all test clean format main.native main.byte

all: main.native

main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) main.native

main.byte: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) main.byte

test: all
	./main.native --test

clean:
	ocamlbuild -clean

format: simulator.ml
	ocamlformat -i --ocp-indent-compat -m 100 simulator.ml
