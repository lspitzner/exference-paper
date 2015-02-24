exference.pdf : exference.md
	pandoc -V geometry:margin=3.7cm --filter pandoc-citeproc exference.md -o exference.pdf

clean:
	rm -fv exference.json exference.odt exference.pdf exference.tex exference.doc exference.log exference.html

