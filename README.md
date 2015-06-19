# bib2sx

`bib2sx` is a command-line tool for parsing BibTeX files (by default) into
S-Expressions.

It also supports outputing to JSON, XML and (canonicalized) BibTeX.
 
A [blog post explaining bib2sx](matt.might.net/articles/parsing-bibtex/)
covers the tool in much more detail.



## Building

To build `bib2sx`, run `make`. 
You will need an installation of the [Racket programming language](http://racket-lang.org/download/). 



## Installation

To install to `~/bin/`, run `make install`.



## Usage

```
 $ bib2sx [ --inline ] [ --json | --bib | --xml ] [ <input.bib> ]
```

where:

 + `--inline` will expand variables into definitions from @string
 + `--flatten` will convert values into properly-quoted BibTeX strings
 + `--json` will output JSON
 + `--bib` will output canonicalized BibTeX
 + `--xml` will output XML

