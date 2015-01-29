OCAMLOPT=ocamlfind opt
OCAMLOPTFLAGS=-g -S -p

OUT=main.native

.SUFFIXES: .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

all: $(OUT)

TARGETS=gopenl.cmx tga.cmx main.cmx

main.native: $(TARGETS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(TARGETS) -o $@

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	$(RM) *.cm[oix] *.o *.s $(OUT)
