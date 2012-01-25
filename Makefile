all:
	ocamlbuild $(ARGS) src/main.native -lib graphics -lib unix
