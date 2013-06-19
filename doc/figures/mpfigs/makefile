ps:
	mpost figure
	mv figure.1 figure1.mps
	mv figure.2 figure2.mps
	mv figure.3 figure3.mps
	mv figure.4 figure4.mps
	mv figure.5 figure5.mps
	mv figure.6 figure6.mps
	mv figure.7 figure7.mps
	mv figure.8 figure8.mps
	mv figure.9 figure9.mps
	latex allfigs
	dvips -o allfigs.ps allfigs.dvi

pdf : allfigs.ps
	pdflatex allfigs

clean :
	rm -f *~ *.log *.aux *.bbl *.blg *.mpx *.out
