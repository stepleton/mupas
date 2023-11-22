# Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

MUPAS = ../../mupas_compiler.py
SRCS := $(wildcard *.pas)

all: perplex.out

perplex.out: $(SRCS)
	$(MUPAS) -O perplex.pas -o $@

clean:
	rm -f perplex.out

.PHONY: all clean
