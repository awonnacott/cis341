DIRS := util,grading,ll,x86
PKGS := num
LIBS := unix,str
SUBMIT := simulator.ml studenttests.ml

.PHONY: all test clean format

all: main.native

main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags -w,+a-4-42 -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) -use-menhir main.native

main.byte: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags -w,+a-4-42 -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) -use-menhir main.byte

test: all
	./main.native --test

clean:
	ocamlbuild -clean
	rm -rf output a.out llstudents.zip

llstudents.zip: cinterop.c studenttests.ml llstudents/*
	zip llstudents cinterop.c studenttests.ml llstudents/*

format: simulator.ml
	ocamlformat -i --ocp-indent-compat -m 100 simulator.ml
