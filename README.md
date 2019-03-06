# googleb-ok

`googleb-ok` extracts text from a PDF and was written with
[Google Books](http://books.google.com) in mind. Google's optical
character recognition has become very good over the last years, even
for works printed in German *black letter*, aka *Gothic letter*
(Frakturschrift). But as long as there's is no good text extraction
tool that can deal with the slight variances of glyph line up of
scanned books, this valuable resource remains unused by text mining
and NLP folks.

[`pdftotext`](https://www.xpdfreader.com/pdftotext-man.html) does a
valuable job in text extraction, but fails with paragraphs in the
wrong order. [`PDFMiner`](https://github.com/euske/pdfminer) performs
much better, but suffers from problems with the inexact lineup of
Glyphs. `googleb-ok` wants to overcome these problems. It's not
intended for the extraction of tabulars, but only of a single text
column. It wouldn't be too hard to extend it to extract two or more
columns.

`googleb-ok` is written in [Haskell](https://www.haskell.org/). It is
a library and a commandline tool. It was written as a generic tool
which can be plugged into an arbitrary PDF parser. To achieve this,
it's functionality is defined in type classes (the most important is
`Glyph`), which can then be instantiated by the data types of a
parser. Right now it can read PDFs using the
[`pdf-toolbox`](https://github.com/Yuras/pdf-toolbox) written in
Haskell or the XML representation of a PDF document, which is yielded
by PDFMiner's `pdf2txt.py -t xml ...` command. The results of the
pipeline `pdfminer -t xml ... | googleb-ok -x` are very promising,
while parsing PDFs directly with the `pdf-toolbox` still suffers from
several deficiencies.

`googleb-ok` is still under heavy development.


## Installation

- 1. Clone this repository
- 2. Clone [`pdf-toolbox`](https://github.com/Yuras/pdf-toolbox) so
  that it lives in the same directory as `googleb-ok`.
- 3. `cd` into the `googleb-ok` directory and run

	stack setup
	stack install --only-dependencies
	stack build
	stack install # optional

If you want to test before installation, then leave the last step and
try the commandline tool from the `googleb-ok` directory using

	stack exec -- googleb-ok --help

`1.8` has shown to be a good choice for the fixed spacing factor.


## TODO/Heuristics
### Spacing
- k-means clustering of inter-glyph spacing per character (I'm working
  on that right now.)
- space before capital letter
- space after comma
- care for spaced letters

### Collect Glyphs of a Line
- try k-means clustering over y-axis instead of
  moving-window-algorithm

### Page Header/Footer
- Number in Headline
- fuzzy match the letters (but not digits) of line against other
  headers/footers.
- line filling < mean

### Syllable division
- Search for a unknown part of bigram overlapping a line break.


## Example

A page from Georg Friedrich Wilhelm Hegel's *Vorlesungen über die
Ästhetic*, edited by Hotho in 1835:

	$ googleb-ok -x -f 1.3 -l 34 -r 1 test/Heg1835a_p205-p207.xml
	178 Erſter Theil. Idee des Kunſtſchönen.
	hören, laſſen wir wie es iſt. Die Organe des Geruchs und Ge
	ſchmacks dagegen gehören ſchon dem Beginne des praktiſchen Ver
	hältniſſes an. Denn zu riechen iſt nur dasjenige, was ſchon im
	Sichverzehren begriffen iſt, und ſchmecken können wir nur, indem
	wir zerſtören. Nun haben wir zwar nur eine Naſe, aber ſie iſt
	zweigetheilt und durchaus in ihren Hälften regelmäßig gebildet.
	Aehnlich iſt es mit den Lippen, Zähnen u. ſ. f. Durchaus regel
	mäßig aber in ihrer Stellung, Geſtalt u. ſ. f. ſind Augen und
	Ohren, und die Glieder für die Ortsverändrung und die Be
	mächtigung und praktiſche Verändrung der äußeren Objekte,
	Beine und Arme.
	Auch im Organiſchen alſo hat die Regelmäßigkeit ihr be
	griffsgemäßes Recht, aber nur bei den Gliedern, welche die Werk
	zeuge für den unmittelbaren Bezug auf die Außenwelt abgeben,
	und nicht den Bezug des Organismus auf ſich ſelbſt als in ſich
	zurückkehrende Subjektivität des Lebens bethätigen.
	Dieß wären die Hauptbeſtimmungen der regelmäßigen und
	ſymmetriſchen Formen und ihrer geſtaltenden Herrſchaft in den
	Naturerſcheinungen.
	Näher nun aber von dieſer abſtrakteren Form iſt
	b) die Geſetzmäßigkeit
	zu unterſcheiden, inſofern ſie ſchon auf einer höheren Stufe ſteht,
	und den Uebergang zu der Freiheit des Lebendigen, ſowohl des
	natürlichen als auch des geiſtigen, ausmacht. Für ſich jedoch be
	trachtet iſt die Geſetzmäßigkeit zwar noch nicht die ſubjektive to
	tale Einheit und Freiheit ſelber, doch iſt ſie bereits eine Tota
	lität weſentlicher Unterſchiede, welche nicht nur als Unter
	ſchiede und Gegenſätze ſich hervorkehren, ſondern in ihrer To
	talität Einheit und Zuſammenhang zeigen. Solche geſetz
	mäßige Einheit und ihre Herrſchaft, obſchon ſie noch im Quan
	titativen ſich geltend macht, iſt nicht mehr auf an ſich ſelbſt äu
	ßerliche und nur zählbare Unterſchiede der bloßen Größe zurück
	zuführen, ſondern läßt ſchon ein qualitatives Verhalten der Y
