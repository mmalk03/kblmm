all: build

build: homework_1

homework_1:
	pdflatex -output-directory=build/homeworks homeworks/tex/1.tex && mv build/homeworks/1.pdf homeworks/pdf/homework_1_mikolaj_malkinski.pdf
