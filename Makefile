PROD=rawparse.ml rawparse.mli parser/rawparse.ml parser/rawparse.mli

all: rawmain.native

rawmain.native: rawmain.ml rawlex.mll rawparse.ml rawparse.mli
	ocamlbuild rawmain.native

rawparse.mli: rawparse.ml
	cat parser/rawparse.mli rawparse.mli.tail > rawparse.mli

rawparse.ml: parser/rawparse.mly
	ocamlyacc parser/rawparse.mly
	cp parser/rawparse.ml .

clean:
	rm $(PROD)
	rm -R _build
