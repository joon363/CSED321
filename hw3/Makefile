CAMLC	 	  = ocamlc
CAMLOPT		= ocamlopt
CAMLCFLAG = 
CAMLDEP=ocamldep

CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex

TARGET = hw3
OBJ = common.cmo hw3.cmo
TEST_OBJ = common.cmo hw3.cmo test.cmo
TEST_TARGET = hw3_test

all: $(OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TARGET) $(OBJ) 

test: $(TEST_OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TEST_TARGET) $(TEST_OBJ) 
	./$(TEST_TARGET)
	
all: $(OBJ)
	$(CAMLC) $(CAMLCFLAG) -o $(TARGET) $(OBJ) 

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

