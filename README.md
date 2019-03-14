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
columns, though.

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

For building and installing [`stack`](https://docs.haskellstack.org),
the haskell build tool, is required.

1. Clone this repository
2. Clone [`pdf-toolbox`](https://github.com/Yuras/pdf-toolbox) so
   that it lives in the same directory as `googleb-ok`.
3. `cd` into the `googleb-ok` directory and run

		stack setup
		stack build
		stack install # optional

The build command will take a while. The first 2 commands will not
install anything outside the `googleb-ok` folder. If you want to test
the app before the installation, then leave out the last step and try
the commandline tool from the `googleb-ok` directory using

	stack exec -- googleb-ok --help

This will show you the options and arguments which you can chose to
optimize the extraction result. E.g. `1.3` has shown to be a good
choice for the fixed spacing factor. See below for an example.

## Usage

`googleb-ok` is a command line program. You definitively have to run

	googleb-ok --help

and read about the command line options. [Here](docs/usage) is the
output of the help command.


## Features / How it works / Heuristics

### Inter-word spacing

The PDF-Format does not even know the concept of spaces.  Adding
inter-word spaces turned out to work good based on a fixed factor: If
the distance to the next glyph exceeds the product of the width and a
fixed spacing factor, then insert a space. The factor may be changed
with a command line argument.

### Collect Glyphs of a Line

The glyphs of a line are collected by sliding window
clustering. Collections of glyphs in text spans found by the pdf
parser are ignored.

The sliding window clustering algorithm outperforms k-means
clustering, which needs the lines count before or needs finding the
first fluctuation...

The window size and stepping of the algorithm may be set using a
command line parameter. For pages with line count above 42 it should
be adapted.

The downside of the sliding window clustering is, that the lines must
not be too skew.

### Categorization of lines

The categorization of lines can be switched on and off. If switched
on, the following categories of lines are parsed base on some
heuristics:

#### Page Header/Footer

- first/last line of a page
- a number is present
- line filling << mean (TODO)

#### Sheet signature (dt. Bogensignatur)

- last line of a page
- indented (adjustable by command line parameters)
- lower font size (TODO)
- line filling << mean (TODO)
- number present (TODO)

#### Custos (dt. Kustode)

- last line of a page
- indented to some bigger portion of the page width (adjustable by
  command line parameters)

#### New paragraph

- indented

#### Block quote

- indented
- lower font size
- spans several lines with same indent (TODO)

#### Default line

- the rest

### Dropping glyphs outside of the type area

- type area (width) is determined by clustering for the start and end
  of the most lines.
- command line toggle for dropping


### Syllable division

- search for an unknown part of bigram overlapping a line break (TODO)


## Example

A page from Georg Friedrich Wilhelm Hegel's *Vorlesungen über die
Ästhetic*, edited by Hotho in 1835,
[scanned by Google](https://books.google.de/books?id=Fss9AQAAMAAJ&hl=de):

![Georg Friedrich Wilhelm Hegel: Vorlesungen über die Ästhetik. Ed. by D.H.G. Hotho, Berlin 1835, vol. 1, p. 205.](docs/Heg1835a_p205.jpg)


That's the output of `googleb-ok`:


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


