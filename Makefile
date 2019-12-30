.Phony: all re fclean clean
NAME = computer
SOURCES =	token.ml	\
			main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt

all: $(NAME)

OBJS = $(SOURCES:.ml = cmo)
OPTOBJS = $(SOURCES:.ml = cmx)

$(NAME): $(OPTOBJS)
	$(CAMLOPT) unix.cmxa -o $(NAME) $(OPTOBJS)

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
