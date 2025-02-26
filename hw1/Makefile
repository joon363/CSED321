CAMLC	 	  = ocamlc
CAMLOPT		= ocamlopt
CAMLCFLAG = 
CAMLDEP=ocamldep

CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex

TARGET = hw1
OBJ = hw1.cmo
TEST_OBJ = hw1.cmo test.cmo

all: $(OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TARGET) $(OBJ) 

test: $(TEST_OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TARGET) $(TEST_OBJ) 
	./$(TARGET)

clean:
	rm -f $(TARGET) *.annot *.aux *.log *.cm[iox] *.dvi 

.SUFFIXES : .ml .mli .cmo .cmi .cmx 

.ml.cmo: $<
	$(CAMLC) $(CAMLCFLAG) -c $< -o $@
.ml.cmx: $<
	$(CAMLOPT) $(CAMLCFLAG) -c $< -o $@
.mli.cmi: $<
	$(CAMLC) $(CAMLCFLAG) -c $< -o $@

depend: 
	$(CAMLDEP) *.mli *.ml > .depend 

.PHONY: depend

include .depend

