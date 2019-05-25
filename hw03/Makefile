DIRS := util,grading,ll,x86
PKGS := num
LIBS := unix,str
CFLAGS := -w,+a-4-26-27-32-33-35-39-40-42-45
SUBMIT := backend.ml studenttests.ml

.PHONY: all test clean format main.native main.byte

all: main.native

main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) -use-menhir main.native

main.byte: $(SUBMIT) $(OTHER)
	ocamlbuild -cflags $(CFLAGS) -Is $(DIRS) -pkg $(PKGS) -libs $(LIBS) -use-menhir main.byte

test: all
	./main.native --test

clean:
	ocamlbuild -clean
	rm -rf output a.out llstudents.zip

llstudents.zip: cinterop.c studenttests.ml llstudents/*
	zip llstudents cinterop.c studenttests.ml llstudents/*

format: backend.ml
	ocamlformat -i --ocp-indent-compat -m 100 backend.ml
