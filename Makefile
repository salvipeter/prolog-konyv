all: prolog-konyv.pdf

prolog-konyv.pdf: konyv.tex
	git rev-parse --short HEAD > revision.tex
	latexmk -pdf $<
