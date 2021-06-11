all: prolog-konyv.pdf

prolog-konyv.pdf: konyv.tex
	latexmk -pdf $<
