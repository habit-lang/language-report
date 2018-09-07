
.SUFFIXES: .pdf .src .tex .lhb .idx .ind .bib .bbl
.PHONY:	   all clean

all:	language.pdf

language.pdf: language.tex language.ind language.bbl

language.tex:	language.src
	./runhaskell.sh build.lhs

.tex.pdf:
	pdflatex $*

.idx.ind:
	makeindex $*

.idx.bbl:
	bibtex $*

.tex.idx:
	pdflatex $*

clean	: 
	-rm *.blg *.bbl *.pdf *.log \
	    *.ind *.ilg *.idx *.aux

