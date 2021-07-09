all: prolog-konyv.pdf

prolog-konyv.pdf: konyv.tex
	git show -s --format=%cs HEAD > revision.tex
	echo " / " >> revision.tex
	git rev-parse --short HEAD >> revision.tex
	latexmk -pdf $<
