# scannedb-ok

`scannedb-ok` extracts text from a PDF and was written with
[Google Books](http://books.google.com) in mind. Google's optical
character recognition has become very good over the last years, even
for works printed in German *black letter*, aka *Gothic letter*
(Frakturschrift). But as long as there's is no good text extraction
tool that can deal with the slight variances in the glyph line up of
scanned books, this valuable resource remains unused by text mining
and NLP folks. `scannedb-ok` was written to always correctly extract
the *lines* of the text and to insert *spaces* correctly even in
complicated cases like emphasis with spaced letters.

[`pdftotext`](https://www.xpdfreader.com/pdftotext-man.html) does a
valuable job in text extraction, but too often fails with lines or
paragraphs in the wrong
order. [`PDFMiner`](https://github.com/euske/pdfminer) performs much
better, but its output suffers from the inexact lineup of Glyphs in
scanned books. In addition, it's hardly possible to identify page
headers, sheet signatures etc. when the PDF has already been
linearized to plain text. `scannedb-ok` wants to overcome these
problems. It's not intended for the extraction of tabulars, but only
of a single text column. It wouldn't be too hard to extend it to
extract two or more columns, though.

## Features

- collects glyphs of each line by a clustering algorithm
- inserts inter-word spaces based on either a rule-based mechanism or
  an artificial neural network (ANN). While the rule-based mechanism works
  in most common cases, the ANN inserts spaces correctly even in
  complicated cases like spaced letters for emphasis.
- optionally drops glyphs outside of the type area
- optionally drops single glyphs between lines
- identifies types of lines: e.g. first line of paragraph, page
  header and footer, sheet signature etc.
- gives options for formatting (indenting) these line types
- repairs syllable division at line breaks (experimental)
- besides plain text output, can
	- generate a word pool for repairing syllable divisions
	- train an ANN that can be used for inserting inter-word spaces
	- output scriptura continua
	- print statistics about each page
	- generate csv with coordinates of glyphs for further analysis

`scannedb-ok` is written in [Haskell](https://www.haskell.org/). It is
a library and a commandline tool. It was written as a generic tool
which can be plugged into an arbitrary PDF parsing library. To achieve
this, it's functionality is defined for
[type classes](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes),
the most important is [`Glyph`](src/Pdf/Extract/Glyph.hs), which can
be instantiated by the data types of a parser. Right now it can read
PDFs using the [`pdf-toolbox`](https://github.com/Yuras/pdf-toolbox)
written in Haskell, or the XML representation of a PDF document, which
is yielded by PDFMiner's `pdf2txt.py -t xml ...` command.

`scannedb-ok` is still under development.


## Installation

For building and installing [`stack`](https://docs.haskellstack.org),
the haskell build tool, is required. Development files for
[`BLAS`](http://www.netlib.org/blas/) and
[`LAPACK`](http://www.netlib.org/lapack/) are also required on the
system. On a Debian-based GNU / Linux system they can be installed
with

	sudo apt install libblas-dev liblapack-dev

Clone this repository and `cd` into the `scannedb-ok` directory and
run

	stack setup
	stack build
	stack install # optional

The build command will take a while. The first 2 commands will not
install anything outside the `scannedb-ok` folder. If you want to test
the app before the installation, then leave out the last step and try
the commandline tool from the `scannedb-ok` directory using

	stack exec -- scannedb-ok --help


## Usage

`scannedb-ok` is a command line program. Please run

	scannedb-ok --help

and read about the commands `scannedb-ok` offers. [Here](docs/usage)
is the output of the help command. There are the following
commands.

- [`text`](docs/usage-text): extract text from a document
- [`words`](docs/usage-words): generate a list of words in a document, but leave possibly
  tokens divided by syllable division on line breaks. It leaves the
  first and the last token of each line.
- [`trainSpacing`](docs/usage-trainSpacing): train and dump an ANN for inserting inter-word
  spaces. This requires at least one *pair of files*, one PDF or XML
  with glyph information and one plain text file containing exactly
  the same text with correct inter-word spaces. One or two pages of
  such training data seem to be enough for good results. The trained
  ANN can be load for the `text` and `words` commands. 
- [`nospaces`](docs/usage-nospaces): extract the text from a document without inserting
  spaces.
- [`stats`](docs/usage-stats): show statistics about each page of a document
- [`spacing`](docs/usage-spacing): print information about inter-glyph distances, glyph
  positions and size for further analysis by external tools
- [`glyphs`](docs/usage-glyphs): print information about the glyphs of a document

To get help about one of these commands, please run

	scannedb-ok COMMAND --help


## How it works / Heuristics

### Inter-word spacing

The PDF-Format does not even know the concept of spaces!  Adding
inter-word spaces turned out to work good based on a fixed factor: If
the distance to the next glyph exceeds the product of the width and a
fixed spacing factor, then insert a space. The factor may be changed
with a command line argument. A reasonable factor is used by default.

While this rule-based mechanism works for most common cases, there are
some printing styles in old books, that require a more sophisticated
algorithm for inserting inter-word spaces. The rule-based mechanism
often fails for spaces letters, which are commonly used for emphasis
from the 16. to 19. centuries.

In order to provide a mechanism that still inserts spaces correctly,
an artificial neural network can be trained. It works on the fact that
the aspect ratios of spaced letter glyphs differ from the aspect
ratios of normal glyphs (at least most of the times in google
books). The ANN evaluates 2 preceding and 2 succeeding glyphs of a
glyph (roughly 50 data points for each glyph).

Here is an example of training a network based on files in the `test`
folder:

	scannedb-ok trainSpacing --momentum 0.9 --iterations 5 -x test/Dre1793.xml test/Dre1793-manual.txt test/Heg1835a_p205-p205.xml test/Heg1835a_p205-p205-fixed1.6.txt net.dat

This will train a network from the file pair `test/Dre1793.xml` and
`test/Dre1793-manual.txt` and then dump it into `net.dat`. The trained
network can be used for text extraction like this:

	scannedb-ok text test/Heg1835a_p205-p205.xml -x -n net.dat

Please note that training a network is based on some randomness. This
means that the precision gained by the network is not always the
same. Sometimes there is no learning progression during all the
training iterations. If this is the case, simply try to rerun the
training command. For the `Dre1793` training data, the trained network
should have a
[precision](https://en.wikipedia.org/wiki/Precision_and_recall) of at
least 98% and recall of at least 99%. For production, do 100 training
iterations at least.


### Collect the glyphs of a Line

The glyphs of a line are collected by sliding window
clustering. Collections of glyphs in text spans found by the pdf
parser are ignored; the clustering is done over the pure bag of glyphs
of a page.

Different from k-means clustering, the sliding window algorithm finds
the number of clusters on its own, provided a suitable window size and
stepping width. These may be set using a command line parameter. For
pages with more than 42 lines it should be adjusted.

The clustering is done on a univariate distribution: the y-coordinates
(heights) of the glyphs' bottoms. The downside of this is that the
lines must not be too skew.

### Categorization of lines

The categorization of lines can be switched on and off. By default,
categorization is switched on. But since it is based on several
clusterings performed for each page (left border, right border, glyph
sizes) it slows down the app. Using the `-C` command line option for
no categorization at all speeds things up.

Right now the categorization is done on the basis of
indentation. Categorization based on line skip will be added in future.

If switched on, the following types of lines are identified on the
base of the following heuristics:

#### Page Header/Footer

- first/last line of a page
- an arabic number is present
- or a roman number is present (TODO)
- line filling << maximal line filling (TODO)
- based on command line options header/footer may be dropped, kept or
  only the number kept

#### Sheet signature (dt. Bogensignatur)

- last line of a page
- indented (adjustable by command line parameters)
- lower font size (TODO)
- line filling << maximal line filling
- number present (TODO)

#### Custos (dt. [Kustode](https://de.wikipedia.org/wiki/Kustode_(Buchherstellung)))

- last line of a page
- indented to some bigger portion of the page width (adjustable by
  command line parameters)

#### Block quote

(still experimental and turned of by default)

- indented
- lower font size
- spans several lines with same indent (TODO)

#### New paragraph

- indented

#### Default line (subsequent lines of a paragraph)

- the rest

### Dropping glyphs outside of the type area

- type area (width) is determined by clustering for the start and end
  of the most lines.
- command line toggle for dropping
- single glyphs above the top or be lower than the bottom line of the
  type area are dropped by the line clustering algorithm:
- a threshold may be defined for the line clustering algorithm. If the
  count of glyphs in a specific height of the page falls below the
  threshold, the glyphs are dropped (customizable by command line
  arguments)
- single inter-line glyphs are dropped by the same line clustering
  algorithm

### Syllable division

- test if the parts of bigram around a line break are known in a
  reference vocabulary
- the first part of the bigram must (or needn't, that's configurable
  by command line options) end with a hyphenation mark, e.g. `-`. Most
  interestingly google books as no hyphenation mark (at least for
  Gothic script).
- if one of them is not known in a reference vocabulary, try to join
  them
- join them, if the joint token is present in the vocabulary

The success of these heuristics depends on the reference
vocabulary. Have Zipf's law in mind: A multitude of tokens occur only
once in your document!

Use the `--tokens` option to generate a list of tokens from the
document without the bigrams around line breaks.

Use the `-w` option to use that list or an other vocabulary as pool of
known tokens. Be warned: A word pool generated from only one document
is to small, because of Zipf's law.

## License

Licensed under either of:

- [BSD-3-Clause license](https://opensource.org/licenses/BSD-3-Clause)
- [Apache License, version 2.0](https://opensource.org/licenses/Apache-2.0)

As a user, you may use this code under either license, at your option.


## Example

A page from Georg Friedrich Wilhelm Hegel's *Vorlesungen über die
Ästhetic*, edited by Hotho in 1835,
[scanned by Google](https://books.google.de/books?id=Fss9AQAAMAAJ&hl=de):

![Georg Friedrich Wilhelm Hegel: Vorlesungen über die Ästhetik. Ed. by D.H.G. Hotho, Berlin 1835, vol. 1, p. 205.](docs/Heg1835a_p205.jpg)


We use PDFMiner to generate an xml representation of the document's
glyphs first. Then we create a word pool without the document header
and table of contents which isn't linearized in an acceptable
way. Then we linearize page 205 of the PDF, which is page 178 of the
book. We use the word pool as a vocabulary for repairing the syllable
division at the line breaks. The `-M` switch drops glyphs outside of
the type area, and it's import to use it in the generation of the word
pool because failed glyphs from the area of the book binding would
disturb the dropping of the bigrams around line breaks. `2>
Heg1835a.log` redirects the logging information printed to
[`stderr`](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
into a log file.

	pdf2txt.py -t xml Heg1835a.pdf > Heg1835a.xml
	scannedb-ok -x -r 30-574 -M --tokens Heg1835.xml > wordpool.txt
	scannedb-ok -x -r 205 -M -w wordpool.txt Heg1835a.xml 2> Heg1835a.log


Here is the output:

	[[178]] hören, laſſen wir wie es iſt. Die Organe des Geruchs und Geſchmacks
	dagegen gehören ſchon dem Beginne des praktiſchen Verhältniſſes
	an. Denn zu riechen iſt nur dasjenige, was ſchon im
	Sichverzehren begriffen iſt, und ſchmecken können wir nur, indem
	wir zerſtören. Nun haben wir zwar nur eine Naſe, aber ſie iſt
	zweigetheilt und durchaus in ihren Hälften regelmäßig gebildet.
	Aehnlich iſt es mit den Lippen, Zähnen u. ſ. f. Durchaus regelmäßig
	aber in ihrer Stellung, Geſtalt u. ſ. f. ſind Augen und
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
	natürlichen als auch des geiſtigen, ausmacht. Für ſich jedoch betrachtet
	iſt die Geſetzmäßigkeit zwar noch nicht die ſubjektive totale
	Einheit und Freiheit ſelber, doch iſt ſie bereits eine Totalität
	weſentlicher Unterſchiede, welche nicht nur als Unterſchiede
	und Gegenſätze ſich hervorkehren, ſondern in ihrer Totalität
	Einheit und Zuſammenhang zeigen. Solche geſetzmäßige
	Einheit und ihre Herrſchaft, obſchon ſie noch im Quan
	titativen ſich geltend macht, iſt nicht mehr auf an ſich ſelbſt äußerliche
	und nur zählbare Unterſchiede der bloßen Größe zurück
	zuführen, ſondern läßt ſchon ein qualitatives Verhalten der



As you can see, some lines have gotten longer--because the syllable
division has been repaired. The log file reports this:


	Joining "Ge" and "ſchmacks".
	Joining "Ver" and "hältniſſes".
	Unable to repair: "im" and "Sichverzehren".
	Unable to repair: "iſt" and "zweigetheilt".
	Joining "regel" and "mäßig".
	Unable to repair: "und" and "Ohren,".
	Unable to repair: "Be" and "mächtigung".
	Unable to repair: "be" and "griffsgemäßes".
	Unable to repair: "Werk" and "zeuge".
	Unable to repair: "ſich" and "zurückkehrende".
	Unable to repair: "den" and "Naturerſcheinungen.
	".
	Unable to repair: "iſt" and "b)".
	Joining "be" and "trachtet".
	Joining "to" and "tale".
	Joining "Tota" and "lität".
	Joining "Unter" and "ſchiede".
	Joining "To" and "talität".
	Joining "geſetz" and "mäßige".
	Unable to repair: "Quan" and "titativen".
	Joining "äu" and "ßerliche".
	Unable to repair: "zurück" and "zuführen,".

`Quantitativen`, `Bemächtigung`, `begriffsmäſiges`, `Werkzeuge`, and
`zurückzuführen` were not in the word pool, so the bigrams weren't
joined together. It's important to understand
[Zipf's law](https://en.wikipedia.org/wiki/Zipf%27s_law) in this
context: You definitively need a better, i.e. bigger word list.

If you use the `-C` option, you will get a pure linearization, without
line categorization. Comparing the outputs you can see that an capital
`Y` in the last line, which was fail-OCRed in the book binding, was
dropped by the `-M` option.

That's the output of `scannedb-ok -x -C -r 205 Heg1835a.xml`:


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


There is still an other command line option dealing with
dropping/not-dropping fail-OCRed glyphs. `-k` keeps the glyphs found
between the lines by the clustering algorithm used for collecting
glyphs into lines. It also works on glyphs between the lines virtually
extended into the margins of the page. The output of `scannedb-ok -x
 -C -r 205 Heg1835a.xml -k` shows a curly brace:

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
	}
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
