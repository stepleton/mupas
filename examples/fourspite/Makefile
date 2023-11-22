# Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

MUPAS = ../../mupas_compiler.py
SRCS := $(wildcard *.pas)

all: fourspite.out

fourspite.out: $(SRCS)
	$(MUPAS) -O fourspite.pas -o $@

clean:
	rm -f fourspite.out

.PHONY: all clean
