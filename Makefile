INSTALLDIR=~/bin/

bib2sx: bib2sx.rkt bibtex/*.rkt
	raco exe bib2sx.rkt

install: bib2sx
	cp bib2sx $(INSTALLDIR)

clean:
	rm -fv bib2sx

