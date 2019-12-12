.Phony: all re fclean clean
NAME = computer
SOURCES = main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt

all: $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml = cmo)
OPTOBJS = $(SOURCES:.ml = cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iiox] *~ .*~ *.o
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).byt
	rm -f $(NAME).opt


re: fclean all
