PROD=parser.ml parser.mli parser/rawparse.ml parser/rawparse.mli

all: rawmain.native

rawmain.native: rawmain.ml rawlex.mll parser.ml parser.mli
	ocamlbuild rawmain.native

parser.mli: parser.ml
	cat parser/rawparse.mli parser/rawparse.mli.tail > parser.mli

parser.ml: parser/rawparse.mly
	ocamlyacc parser/rawparse.mly
	cp parser/rawparse.ml parser.ml

clean:
	rm $(PROD)
	rm -R _build
