# bib2sx

`bib2sx` is a command-line tool for parsing BibTeX files (by default) into
S-Expressions.

It also supports outputing to JSON, XML and (canonicalized) BibTeX.
 

## Building

To build `bib2sx`, run `make`.



## Installation

To install to `~/bin/`, run `make install`.



## Usage

```
 $ bib2sx [ --inline ] [ --json | --bib | --xml ] [ <input.bib> ]
```

where:

 + `--inline` will expand variables into definitions from @string
 + `--json` will output JSON
 + `--bib` will output canonicalized BibTeX
 + `--xml` will output XML

