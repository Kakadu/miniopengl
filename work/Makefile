OCAMLOPT=ocamlfind opt
OCAMLOPTFLAGS=-package str -g -S -p -inline 1

OUT=main.native
OUT_TEST1=test1.native
OUT_ALL=$(OUT) $(OUT_TEST1)

.SUFFIXES: .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

all: $(OUT)

LIB_TARGETS=gopenl.cmx tga.cmx wavefront.cmx
TARGETS=$(LIB_TARGETS) main.cmx

main.native: $(TARGETS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkpkg $^ -o $@

$(OUT_TEST1): $(LIB_TARGETS) test1.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $^ -o $@

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

tga.cmx: gopenl.cmx
main.cmx: gopenl.cmx

clean:
	$(RM) *.cm[oix] *.o *.s $(OUT_ALL)
