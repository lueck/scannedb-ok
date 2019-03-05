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
