CAMLC	 	  = ocamlc
CAMLOPT		= ocamlopt
CAMLCFLAG = 
CAMLDEP=ocamldep

CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex

TARGET = hw2
OBJ = hw2.cmo
TEST_OBJ = hw2.cmo test.cmo
TEST_TARGET = hw2_test

all: $(OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TARGET) $(OBJ) 

test: $(TEST_OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TEST_TARGET) $(TEST_OBJ) 
	./$(TEST_TARGET)

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

