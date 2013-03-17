# (C) 2011-13 Nicola Bonelli <bonelli@antifork.org>
#

GHCFLAGS= --make -O2 -Wall 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: git-prompt 

git-prompt: git-prompt.hs
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		cp git-prompt  ${INSTDIR}/bin/
        
clean:
	   @rm -f git-prompt
	   @rm -f *.o *.hi
